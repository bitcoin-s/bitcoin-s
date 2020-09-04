package org.bitcoins.dlc.wallet

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{AddressType, BIP32Path, HDChainType}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BlockStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution._
import org.bitcoins.dlc.sign.DLCTxSigner
import org.bitcoins.dlc.verify.DLCSignatureVerifier
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletLogger}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

abstract class DLCWallet extends Wallet with AnyDLCHDWalletApi {

  implicit val dlcConfig: DLCAppConfig

  private[bitcoins] val dlcOfferDAO: DLCOfferDAO = DLCOfferDAO()
  private[bitcoins] val dlcAcceptDAO: DLCAcceptDAO = DLCAcceptDAO()
  private[bitcoins] val dlcDAO: DLCDAO = DLCDAO()
  private[bitcoins] val dlcInputsDAO: DLCFundingInputDAO = DLCFundingInputDAO()
  private[bitcoins] val dlcSigsDAO: DLCCETSignatureDAO = DLCCETSignatureDAO()
  private[bitcoins] val dlcRefundSigDAO: DLCRefundSigDAO = DLCRefundSigDAO()

  private def calcContractId(
      offer: DLCOffer,
      accept: DLCAccept): Future[ByteVector] = {
    val builder = DLCTxBuilder(offer, accept.withoutSigs)
    builder.buildFundingTx.map(_.txIdBE.bytes.xor(accept.tempContractId.bytes))
  }

  private def initDLC(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean): Future[DLCDb] = {
    dlcDAO.findByParamHash(paramHash).flatMap {
      case Some(dlcDb) =>
        Future.successful(dlcDb)
      case None =>
        val state = if (isInitiator) {
          DLCState.Offered
        } else DLCState.Accepted

        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          nextIndex <- getNextAvailableIndex(account, HDChainType.External)
          dlc = {
            DLCDb(
              paramHash = paramHash,
              tempContractIdOpt = None,
              contractIdOpt = None,
              state = state,
              isInitiator = isInitiator,
              account = account.hdAccount,
              keyIndex = nextIndex,
              oracleSigOpt = None
            )
          }
          _ <- writeDLCKeysToAddressDb(account, nextIndex)
          writtenDLC <- dlcDAO.create(dlc)
        } yield writtenDLC
    }
  }

  private def updateDLCTempContractId(
      paramHash: Sha256DigestBE,
      tempContractId: Sha256DigestBE): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.read(paramHash)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with paramHash ${paramHash.hex}"))
      }
      updated <-
        dlcDAO.update(dlcDb.copy(tempContractIdOpt = Some(tempContractId)))
    } yield updated
  }

  private def updateDLCContractIds(
      offer: DLCOffer,
      accept: DLCAccept): Future[DLCDb] = {
    require(accept.tempContractId == offer.tempContractId,
            "Offer and Accept have differing tempContractIds!")
    val paramHash = offer.paramHash
    for {
      dlcOpt <- dlcDAO.read(paramHash)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with paramHash ${paramHash.hex}"))
      }
      contractId <- calcContractId(offer, accept)

      newDLCDb = dlcDb.copy(contractIdOpt = Some(contractId),
                            tempContractIdOpt = Some(accept.tempContractId))

      updated <- dlcDAO.update(newDLCDb)
    } yield updated
  }

  private def updateDLCState(
      paramHash: Sha256DigestBE,
      state: DLCState): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.read(paramHash)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with paramHash ${paramHash.hex}"))
      }
      updated <- dlcDAO.update(dlcDb.updateState(state))
    } yield updated
  }

  private def updateDLCState(
      contractId: ByteVector,
      state: DLCState): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with contractId ${contractId.toHex}"))
      }
      updated <- dlcDAO.update(dlcDb.updateState(state))
    } yield updated
  }

  private def updateDLCOracleSig(
      contractId: ByteVector,
      sig: SchnorrDigitalSignature): Future[DLCDb] = {
    dlcDAO.findByContractId(contractId).flatMap {
      case Some(dlcDb) =>
        dlcDAO.update(dlcDb.copy(oracleSigOpt = Some(sig)))
      case None =>
        Future.failed(
          new NoSuchElementException(
            s"No DLC found with that contractId ${contractId.toHex}"))
    }
  }

  private def calcDLCPubKeys(
      xpub: ExtPublicKey,
      keyIndex: Int): DLCPublicKeys = {
    val fundingKey =
      xpub
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/$keyIndex"))
        .get
        .key

    val payoutKey =
      xpub
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/${keyIndex + 1}"))
        .get
        .key

    networkParameters match {
      case bitcoinNetwork: BitcoinNetwork =>
        DLCPublicKeys.fromPubKeys(fundingKey, payoutKey, bitcoinNetwork)
    }
  }

  private def writeDLCKeysToAddressDb(
      account: AccountDb,
      index: Int): Future[Vector[AddressDb]] = {
    for {
      zero <- getAddress(account, HDChainType.External, index)
      one <- getAddress(account, HDChainType.External, index + 1)
    } yield {
      Vector(zero, one)
    }
  }

  /**
    * Creates a DLCOffer, if one has already been created
    * with the given parameters then that one will be returned instead.
    *
    * This is the first step of the initiator
    */
  override def createDLCOffer(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[FeeUnit],
      locktime: UInt32,
      refundLocktime: UInt32): Future[DLCOffer] = {
    logger.debug("Calculating relevant wallet data for DLC Offer")

    val timeouts =
      DLCTimeouts(BlockStamp(locktime.toInt), BlockStamp(refundLocktime.toInt))

    val paramHash = DLCMessage.calcParamHash(oracleInfo, contractInfo, timeouts)

    logger.debug(
      s"Checking if DLC Offer has already been made (${paramHash.hex})")

    for {
      feeRate <- determineFeeRate(feeRateOpt)
      satoshisPerVirtualByte = SatoshisPerVirtualByte(feeRate.currencyUnit)
      dlc <- initDLC(paramHash = paramHash, isInitiator = true)
      dlcOfferDbOpt <- dlcOfferDAO.findByParamHash(paramHash)
      dlcOffer <- dlcOfferDbOpt match {
        case Some(dlcOfferDb) =>
          logger.debug(
            s"DLC Offer (${paramHash.hex}) has already been made, returning offer")

          dlcInputsDAO.findByParamHash(paramHash, isInitiator = true).map {
            fundingInputs =>
              val inputRefs = fundingInputs.map(_.toOutputReference)
              dlcOfferDb.toDLCOffer(inputRefs)
          }
        case None =>
          createNewDLCOffer(
            dlc = dlc,
            collateral = collateral,
            oracleInfo = oracleInfo,
            contractInfo = contractInfo,
            feeRate = satoshisPerVirtualByte,
            timeouts = timeouts
          )
      }
    } yield dlcOffer
  }

  private def createNewDLCOffer(
      dlc: DLCDb,
      collateral: CurrencyUnit,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts): Future[DLCOffer] = {
    for {
      accountOpt <- accountDAO.findByAccount(dlc.account)
      account = accountOpt.get
      (txBuilder, spendingInfos) <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        fromTagOpt = None,
        markAsReserved = true
      )
      utxos = spendingInfos.map(_.outputReference)

      changeSPK =
        txBuilder.finalizer.changeSPK
          .asInstanceOf[WitnessScriptPubKey]
      network = networkParameters.asInstanceOf[BitcoinNetwork]
      changeAddr = Bech32Address(changeSPK, network)

      dlcPubKeys = calcDLCPubKeys(account.xpub, dlc.keyIndex)

      _ = logger.debug(
        s"DLC Offer data collected, creating database entry, ${dlc.paramHash.hex}")

      dlcOfferDb = DLCOfferDb(
        dlc.paramHash,
        oraclePubKey = oracleInfo.pubKey,
        oracleRValue = oracleInfo.rValue,
        contractInfo = contractInfo,
        contractMaturity = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout,
        fundingKey = dlcPubKeys.fundingKey,
        payoutAddress = dlcPubKeys.payoutAddress,
        totalCollateral = collateral,
        feeRate = feeRate,
        changeAddress = changeAddr
      )

      dlcInputs = utxos.map(outRef =>
        DLCFundingInputDb(
          paramHash = dlc.paramHash,
          isInitiator = true,
          outPoint = outRef.outPoint,
          output = outRef.output,
          redeemScriptOpt = None, // todo negotiate these
          witnessScriptOpt = None,
          sigs = Vector.empty
        ))

      _ <- dlcInputsDAO.createAll(dlcInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)

      offer = dlcOfferDb.toDLCOffer(utxos)

      _ <- updateDLCTempContractId(paramHash = offer.paramHash,
                                   offer.tempContractId)
    } yield offer
  }

  /**
    * Creates a DLCAccept from the default Segwit account from a given offer, if one has already been
    * created with the given parameters then that one will be returned instead.
    *
    * This is the first step of the recipient
    */
  override def acceptDLCOffer(offer: DLCOffer): Future[DLCAccept] = {
    logger.debug("Calculating relevant wallet data for DLC Accept")

    val paramHash = offer.paramHash

    val collateral = offer.contractInfo.values.max - offer.totalCollateral

    logger.debug(s"Checking if Accept (${paramHash.hex}) has already been made")
    for {
      dlc <- initDLC(paramHash = paramHash, isInitiator = false)
      accountOpt <- accountDAO.findByAccount(dlc.account)
      dlcAcceptDbOpt <- dlcAcceptDAO.findByParamHash(paramHash)
      dlcAccept <- dlcAcceptDbOpt match {
        case Some(dlcAcceptDb) =>
          logger.debug(
            s"DLC Accept (${paramHash.hex}) has already been made, returning accept")
          for {
            fundingInputs <-
              dlcInputsDAO.findByParamHash(paramHash, isInitiator = false)
            outcomeSigDbs <- dlcSigsDAO.findByParamHash(paramHash)
            refundSigDb <- dlcRefundSigDAO.read(paramHash, false)
          } yield {
            val inputRefs = fundingInputs.map(_.toOutputReference)
            val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap

            dlcAcceptDb.toDLCAccept(offer.tempContractId,
                                    inputRefs,
                                    outcomeSigs,
                                    refundSigDb.get.refundSig)
          }
        case None =>
          createNewDLCAccept(dlc, accountOpt.get, collateral, offer)
      }
    } yield dlcAccept
  }

  private def createNewDLCAccept(
      dlc: DLCDb,
      account: AccountDb,
      collateral: CurrencyUnit,
      offer: DLCOffer): Future[DLCAccept] = {
    for {
      (txBuilder, spendingInfos) <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = offer.feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        fromTagOpt = None,
        markAsReserved = true
      )
      network = networkParameters.asInstanceOf[BitcoinNetwork]

      utxos = spendingInfos.map(_.outputReference)

      changeSPK = txBuilder.finalizer.changeSPK.asInstanceOf[P2WPKHWitnessSPKV0]
      changeAddr = Bech32Address(changeSPK, network)

      // todo change to a ExtSign.deriveAndSignFuture
      extPrivKey =
        keyManager.rootExtPrivKey.deriveChildPrivKey(account.hdAccount)

      dlcPubKeys = calcDLCPubKeys(account.xpub, dlc.keyIndex)

      fundingPrivKey =
        extPrivKey
          .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${dlc.keyIndex}"))
          .key

      _ = require(dlcPubKeys.fundingKey == fundingPrivKey.publicKey,
                  "Did not derive the same funding private and public key")

      acceptWithoutSigs = DLCAcceptWithoutSigs(
        totalCollateral = collateral.satoshis,
        pubKeys = dlcPubKeys,
        fundingInputs = utxos,
        changeAddress = changeAddr,
        tempContractId = offer.tempContractId
      )

      builder = DLCTxBuilder(offer, acceptWithoutSigs)

      signer = DLCTxSigner(builder = builder,
                           isInitiator = false,
                           fundingKey = fundingPrivKey,
                           finalAddress = dlcPubKeys.payoutAddress,
                           fundingUtxos = spendingInfos)

      cetSigs <- signer.createCETSigs()

      _ = logger.debug(
        s"DLC Accept data collected, creating database entry, ${dlc.paramHash.hex}")

      dlcAcceptDb = DLCAcceptDb(
        paramHash = dlc.paramHash,
        fundingKey = dlcPubKeys.fundingKey,
        finalAddress = dlcPubKeys.payoutAddress,
        totalCollateral = collateral,
        changeAddress = changeAddr
      )

      sigsDbs = cetSigs.outcomeSigs.map(sig =>
        DLCCETSignatureDb(dlc.paramHash, sig._1, sig._2))

      refundSigDb =
        DLCRefundSigDb(dlc.paramHash, isInitiator = false, cetSigs.refundSig)

      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(offer)

      offerInputs = offer.fundingInputs.map(outRef =>
        DLCFundingInputDb(
          paramHash = dlc.paramHash,
          isInitiator = true,
          outPoint = outRef.outPoint,
          output = outRef.output,
          redeemScriptOpt = None, // todo negotiate these
          witnessScriptOpt = None,
          sigs = Vector.empty
        ))

      acceptInputs = spendingInfos.map(utxo =>
        DLCFundingInputDb(
          paramHash = dlc.paramHash,
          isInitiator = false,
          outPoint = utxo.outPoint,
          output = utxo.output,
          redeemScriptOpt = InputInfo.getRedeemScript(utxo.inputInfo),
          witnessScriptOpt = InputInfo.getScriptWitness(utxo.inputInfo),
          sigs = Vector.empty
        ))

      accept = dlcAcceptDb.toDLCAccept(offer.tempContractId,
                                       utxos,
                                       cetSigs.outcomeSigs,
                                       cetSigs.refundSig)

      _ = require(accept.tempContractId == offer.tempContractId,
                  "Offer and Accept have differing tempContractIds!")

      _ <- dlcInputsDAO.createAll(offerInputs ++ acceptInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
      _ <- dlcAcceptDAO.create(dlcAcceptDb)
      _ <- dlcSigsDAO.createAll(sigsDbs.toVector)
      _ <- dlcRefundSigDAO.create(refundSigDb)
      _ <- updateDLCContractIds(offer, accept)
    } yield accept
  }

  def registerDLCAccept(accept: DLCAccept): Future[DLCDb] = {
    dlcDAO.findByTempContractId(accept.tempContractId).flatMap {
      case Some(dlc) =>
        require(
          dlc.isInitiator,
          s"We cannot register a DLCAccept if we are not the initiator, got $dlc")

        logger.debug(
          s"DLC Offer (${dlc.paramHash.hex}) found, adding accept data")

        val paramHash = dlc.paramHash
        val dlcAcceptDb = DLCAcceptDbHelper.fromDLCAccept(paramHash, accept)
        val acceptInputs = accept.fundingInputs.map(outRef =>
          DLCFundingInputDb(
            paramHash = paramHash,
            isInitiator = false,
            outPoint = outRef.outPoint,
            output = outRef.output,
            redeemScriptOpt = None, // todo negotiate these
            witnessScriptOpt = None,
            sigs = Vector.empty
          ))
        val sigsDbs = accept.cetSigs.outcomeSigs
          .map(sig => DLCCETSignatureDb(paramHash, sig._1, sig._2))
          .toVector

        val refundSigDb = DLCRefundSigDb(paramHash,
                                         isInitiator = false,
                                         accept.cetSigs.refundSig)

        for {
          isCETSigsValid <- verifyCETSigs(accept)
          _ = if (!isCETSigsValid)
            throw new IllegalArgumentException(
              s"CET sigs provided are not valid! got ${accept.cetSigs.outcomeSigs}")
          isRefundSigValid <- verifyRefundSig(accept)
          _ = if (!isRefundSigValid)
            throw new IllegalArgumentException(
              s"Refund sig provided is not valid! got ${accept.cetSigs.refundSig}")
          _ <- dlcInputsDAO.upsertAll(acceptInputs)
          _ <- dlcSigsDAO.upsertAll(sigsDbs)
          _ <- dlcRefundSigDAO.upsert(refundSigDb)
          _ <- dlcAcceptDAO.upsert(dlcAcceptDb)
          _ <- dlcDAO.update(dlc.updateState(DLCState.Accepted))

          // .get is safe here because we must have an offer if we have a dlcDAO
          offerDb <- dlcOfferDAO.findByParamHash(dlc.paramHash).map(_.get)
          offerInputs <-
            dlcInputsDAO.findByParamHash(dlc.paramHash, isInitiator = true)
          offer = offerDb.toDLCOffer(offerInputs.map(_.toOutputReference))

          updatedDLCDb <- updateDLCContractIds(offer, accept)
        } yield updatedDLCDb
      case None =>
        throw new RuntimeException(
          s"No DLC Offer found with corresponding tempContractId ${accept.tempContractId.hex}, this wallet did not create the corresponding offer")
    }
  }

  /**
    * Creates signatures for the DLCs CETs and Funding Inputs
    *
    * This is the second step of the initiator
    */
  override def signDLC(accept: DLCAccept): Future[DLCSign] = {
    for {
      dlc <- registerDLCAccept(accept)
      signer <- signerFromDb(dlc.paramHash)

      cetSigs <- signer.createCETSigs()
      fundingSigs <- signer.createFundingTxSigs()

      refundSigDb =
        DLCRefundSigDb(dlc.paramHash, isInitiator = true, cetSigs.refundSig)
      _ <- dlcRefundSigDAO.upsert(refundSigDb)

      _ <- updateDLCState(dlc.paramHash, DLCState.Signed)
    } yield {
      // _.get is safe, must be defined by now
      DLCSign(cetSigs, fundingSigs, dlc.contractIdOpt.get)
    }
  }

  def verifyCETSigs(accept: DLCAccept): Future[Boolean] = {
    verifierFromAccept(accept).map { verifier =>
      val correctNumberOfSigs =
        accept.cetSigs.outcomeSigs.size == verifier.builder.offerOutcomes.size

      correctNumberOfSigs && accept.cetSigs.outcomeSigs.foldLeft(true) {
        case (ret, (outcome, sig)) =>
          ret && verifier.verifyCETSig(outcome, sig)
      }
    }
  }

  def verifyCETSigs(
      paramHash: Sha256DigestBE,
      sign: DLCSign): Future[Boolean] = {
    verifierFromDb(paramHash).map { verifier =>
      val correctNumberOfSigs =
        sign.cetSigs.outcomeSigs.size == verifier.builder.offerOutcomes.size

      correctNumberOfSigs && sign.cetSigs.outcomeSigs.foldLeft(true) {
        case (ret, (outcome, sig)) =>
          ret && verifier.verifyCETSig(outcome, sig)
      }
    }
  }

  def verifyRefundSig(accept: DLCAccept): Future[Boolean] = {
    verifierFromAccept(accept).map { verifier =>
      verifier.verifyRefundSig(accept.cetSigs.refundSig)
    }
  }

  def verifyRefundSig(
      paramHash: Sha256DigestBE,
      sign: DLCSign): Future[Boolean] = {
    verifierFromDb(paramHash).map { verifier =>
      verifier.verifyRefundSig(sign.cetSigs.refundSig)
    }
  }

  def verifyFundingSigs(
      paramHash: Sha256DigestBE,
      inputs: Vector[DLCFundingInputDb],
      sign: DLCSign): Future[Boolean] = {
    if (inputs.count(!_.isInitiator) == sign.fundingSigs.keys.size) {
      verifierFromDb(paramHash).map { verifier =>
        verifier.verifyRemoteFundingSigs(sign.fundingSigs)
      }
    } else {
      logger.info(
        "Funding Signatures provided did not have the correct amount of inputs")
      Future.successful(false)
    }
  }

  /** Takes a DLCSign an inserts the funding signatures into the database
    * This is the only way one should insert sigs to the database
    */
  def addFundingSigs(sign: DLCSign): Future[Vector[DLCFundingInputDb]] = {
    for {
      dlc <- dlcDAO.findByContractId(sign.contractId).map(_.get)
      inputs <- dlcInputsDAO.findByParamHash(dlc.paramHash)
      isValid <- verifyFundingSigs(paramHash = dlc.paramHash,
                                   inputs = inputs,
                                   sign = sign)
      _ <- {
        if (!isValid)
          Future.failed(new IllegalArgumentException(
            s"Funding Signatures provided are not valid! got ${sign.fundingSigs}"))
        else FutureUtil.unit
      }

      updatedInputs = sign.fundingSigs.map {
        case (outPoint, sigs) =>
          inputs.find(_.outPoint == outPoint) match {
            case Some(inputDb) =>
              inputDb.copy(sigs = sigs)
            case None =>
              throw new NoSuchElementException(
                s"Received signature for outPoint (${outPoint.hex}) that does not correspond to this contractId (${sign.contractId.toHex})")
          }
      }
      written <- dlcInputsDAO.upsertAll(updatedInputs.toVector)
    } yield written
  }

  /**
    * Inputs the received signatures for a DLC into our database
    *
    * This is the second step of the recipient
    */
  override def addDLCSigs(sign: DLCSign): Future[DLCDb] = {
    dlcDAO.findByContractId(sign.contractId).flatMap {
      case Some(dlc) =>
        val refundSigDb = DLCRefundSigDb(dlc.paramHash,
                                         isInitiator = true,
                                         sign.cetSigs.refundSig)
        val sigsDbs = sign.cetSigs.outcomeSigs
          .map(sig => DLCCETSignatureDb(dlc.paramHash, sig._1, sig._2))
          .toVector

        for {
          isRefundSigValid <- verifyRefundSig(dlc.paramHash, sign)
          _ = if (!isRefundSigValid)
            throw new IllegalArgumentException(
              s"Refund sig provided is not valid! got ${sign.cetSigs.refundSig}")

          isCETSigsValid <- verifyCETSigs(dlc.paramHash, sign)
          _ = if (!isCETSigsValid)
            throw new IllegalArgumentException(
              s"CET sigs provided are not valid! got ${sign.cetSigs.outcomeSigs}")
          _ <- dlcSigsDAO.createAll(sigsDbs)
          _ <- dlcRefundSigDAO.create(refundSigDb)
          _ <- addFundingSigs(sign)
          updated <- dlcDAO.update(dlc.updateState(DLCState.Signed))
        } yield updated
      case None =>
        Future.failed(new NoSuchElementException(
          s"No DLC found with corresponding contractId ${sign.contractId.toHex}"))
    }
  }

  private def getAllDLCData(contractId: ByteVector): Future[
    (
        DLCDb,
        DLCOfferDb,
        DLCAcceptDb,
        Vector[DLCRefundSigDb],
        Vector[DLCFundingInputDb],
        Vector[DLCCETSignatureDb])] = {
    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb = dlcDbOpt.get
      (_, dlcOffer, dlcAccept, refundSigs, fundingInputs, outcomeSigs) <-
        getAllDLCData(dlcDb.paramHash)
    } yield (dlcDb, dlcOffer, dlcAccept, refundSigs, fundingInputs, outcomeSigs)
  }

  private def getAllDLCData(paramHash: Sha256DigestBE): Future[
    (
        DLCDb,
        DLCOfferDb,
        DLCAcceptDb,
        Vector[DLCRefundSigDb],
        Vector[DLCFundingInputDb],
        Vector[DLCCETSignatureDb])] = {
    for {
      dlcDbOpt <- dlcDAO.findByParamHash(paramHash)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByParamHash(paramHash)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByParamHash(paramHash)
      dlcAccept = dlcAcceptOpt.get
      refundSigs <- dlcRefundSigDAO.findByParamHash(paramHash)
      fundingInputs <- dlcInputsDAO.findByParamHash(paramHash)
      outcomeSigs <- dlcSigsDAO.findByParamHash(paramHash)
    } yield (dlcDb, dlcOffer, dlcAccept, refundSigs, fundingInputs, outcomeSigs)
  }

  private def fundingUtxosFromDb(
      dlcDb: DLCDb,
      fundingInputs: Vector[DLCFundingInputDb]): Future[
    Vector[ScriptSignatureParams[InputInfo]]] = {
    val outPoints =
      fundingInputs.filter(_.isInitiator == dlcDb.isInitiator).map(_.outPoint)

    for {
      utxos <- listUtxos(outPoints)
      scriptSigParams <-
        FutureUtil.foldLeftAsync(Vector.empty[ScriptSignatureParams[InputInfo]],
                                 utxos) { (accum, utxo) =>
          transactionDAO
            .findByOutPoint(utxo.outPoint)
            .map(txOpt =>
              utxo.toUTXOInfo(keyManager, txOpt.get.transaction) +: accum)
        }
    } yield scriptSigParams
  }

  private def verifierFromAccept(
      accept: DLCAccept): Future[DLCSignatureVerifier] = {
    for {
      dlcDbOpt <- dlcDAO.findByTempContractId(accept.tempContractId)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByParamHash(dlcDb.paramHash)
      dlcOffer = dlcOfferOpt.get
      fundingInputsDb <- dlcInputsDAO.findByParamHash(dlcDb.paramHash)
    } yield {
      val offerFundingInputs =
        fundingInputsDb.filter(_.isInitiator).map(_.toOutputReference)

      val builder =
        DLCTxBuilder(dlcOffer.toDLCOffer(offerFundingInputs),
                     accept.withoutSigs)

      DLCSignatureVerifier(builder, dlcDb.isInitiator)
    }
  }

  private def verifierFromDb(
      paramHash: Sha256DigestBE): Future[DLCSignatureVerifier] = {
    getAllDLCData(paramHash).map {
      case (dlcDb, dlcOffer, dlcAccept, _, fundingInputsDb, _) =>
        val offerFundingInputs =
          fundingInputsDb.filter(_.isInitiator).map(_.toOutputReference)
        val acceptFundingInputs =
          fundingInputsDb.filterNot(_.isInitiator).map(_.toOutputReference)

        val offer = dlcOffer.toDLCOffer(offerFundingInputs)
        val accept = dlcAccept.toDLCAcceptWithoutSigs(offer.tempContractId,
                                                      acceptFundingInputs)

        val builder = DLCTxBuilder(offer, accept)

        DLCSignatureVerifier(builder, dlcDb.isInitiator)
    }
  }

  private def signerFromDb(paramHash: Sha256DigestBE): Future[DLCTxSigner] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, _, fundingInputsDb, _) <- getAllDLCData(
        paramHash)
      signer <- signerFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputsDb)
    } yield signer
  }

  private def signerFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb]): Future[DLCTxSigner] = {
    fundingUtxosFromDb(dlcDb, fundingInputsDb).map { fundingUtxos =>
      val offerFundingInputs =
        fundingInputsDb.filter(_.isInitiator).map(_.toOutputReference)
      val acceptFundingInputs =
        fundingInputsDb.filterNot(_.isInitiator).map(_.toOutputReference)

      val offer = dlcOffer.toDLCOffer(offerFundingInputs)
      val accept = dlcAccept.toDLCAcceptWithoutSigs(offer.tempContractId,
                                                    acceptFundingInputs)

      val builder = DLCTxBuilder(offer, accept)

      val extPrivKey =
        keyManager.rootExtPrivKey.deriveChildPrivKey(dlcDb.account)

      val (fundingKey, payoutAddress) = if (dlcDb.isInitiator) {
        (dlcOffer.fundingKey, dlcOffer.payoutAddress)
      } else {
        (dlcAccept.fundingKey, dlcAccept.finalAddress)
      }

      val fundingPrivKey =
        extPrivKey
          .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${dlcDb.keyIndex}"))
          .key

      require(fundingKey == fundingPrivKey.publicKey)

      DLCTxSigner(builder = builder,
                  isInitiator = dlcDb.isInitiator,
                  fundingKey = fundingPrivKey,
                  finalAddress = payoutAddress,
                  fundingUtxos = fundingUtxos)
    }
  }

  private def executorFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb]): Future[DLCExecutor] = {
    signerFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputsDb).map(
      DLCExecutor.apply)
  }

  private def executorAndSetupFromDb(
      contractId: ByteVector): Future[(DLCExecutor, SetupDLC)] = {
    getAllDLCData(contractId).flatMap {
      case (dlcDb,
            dlcOffer,
            dlcAccept,
            refundSigs,
            fundingInputsDb,
            outcomeSigDbs) =>
        executorAndSetupFromDb(dlcDb,
                               dlcOffer,
                               dlcAccept,
                               refundSigs,
                               fundingInputsDb,
                               outcomeSigDbs)
    }
  }

  private def executorAndSetupFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      refundSigs: Vector[DLCRefundSigDb],
      fundingInputs: Vector[DLCFundingInputDb],
      outcomeSigDbs: Vector[DLCCETSignatureDb]): Future[
    (DLCExecutor, SetupDLC)] = {

    executorFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputs)
      .flatMap { executor =>
        val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap

        val refundSig =
          refundSigs.find(_.isInitiator == !dlcDb.isInitiator).get.refundSig
        val setupF = if (dlcDb.isInitiator) {
          // Note that the funding tx in this setup is not signed
          val cetSigs = CETSignatures(outcomeSigs, refundSig)
          executor.setupDLCOffer(cetSigs)
        } else {
          val cetSigs = CETSignatures(outcomeSigs, refundSig)
          val fundingSigs =
            fundingInputs
              .filter(_.isInitiator)
              .map(input => (input.outPoint, input.sigs))
              .toMap
          executor.setupDLCAccept(cetSigs, FundingSignatures(fundingSigs))
        }

        setupF.map((executor, _))
      }
  }

  override def getDLCFundingTx(contractId: ByteVector): Future[Transaction] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, _, fundingInputs, _) <- getAllDLCData(
        contractId)

      signer <- signerFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputs)
      fundingTx <- {
        if (dlcDb.isInitiator) {
          // TODO: If this is called after seeing the funding tx on-chain, it should return that one
          signer.builder.buildFundingTx
        } else {
          val remoteSigs = fundingInputs
            .filter(_.isInitiator)
            .map(input => (input.outPoint, input.sigs))
            .toMap
          signer.signFundingTx(FundingSignatures(remoteSigs))
        }
      }
    } yield fundingTx
  }

  override def broadcastDLCFundingTx(
      contractId: ByteVector): Future[Transaction] = {
    for {
      tx <- getDLCFundingTx(contractId)
      _ <- broadcastTransaction(tx)

      _ <- updateDLCState(contractId, DLCState.Broadcasted)

    } yield tx
  }

  override def executeDLC(
      contractId: ByteVector,
      oracleSig: SchnorrDigitalSignature): Future[Transaction] = {
    for {
      _ <- updateDLCOracleSig(contractId, oracleSig)

      (executor, setup) <- executorAndSetupFromDb(contractId)

      payout = executor.getPayout(oracleSig)
      _ = if (payout <= 0.satoshis)
        throw new UnsupportedOperationException(
          "Cannot execute a losing outcome")

      outcome <- executor.executeDLC(setup, oracleSig)

      _ <- updateDLCState(contractId, DLCState.Claimed)
    } yield {
      outcome.cet
    }
  }

  override def executeDLCRefund(contractId: ByteVector): Future[Transaction] = {
    for {
      (executor, setup) <- executorAndSetupFromDb(contractId)
      _ <- updateDLCState(contractId, DLCState.Refunded)
    } yield {
      val outcome = executor.executeRefundDLC(setup)
      outcome.refundTx
    }
  }

}

object DLCWallet extends WalletLogger {

  private case class DLCWalletImpl(
      keyManager: BIP39KeyManager,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi,
      override val creationTime: Instant
  )(implicit
      val walletConfig: WalletAppConfig,
      val dlcConfig: DLCAppConfig,
      val ec: ExecutionContext
  ) extends DLCWallet

  def apply(
      keyManager: BIP39KeyManager,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi,
      creationTime: Instant)(implicit
      config: WalletAppConfig,
      dlcConfig: DLCAppConfig,
      ec: ExecutionContext): DLCWallet = {
    DLCWalletImpl(keyManager, nodeApi, chainQueryApi, feeRateApi, creationTime)
  }
}
