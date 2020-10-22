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
import org.bitcoins.core.wallet.utxo._
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
  private[bitcoins] val remoteTxDAO: DLCRemoteTxDAO = DLCRemoteTxDAO()

  private def calcContractId(
      offer: DLCOffer,
      accept: DLCAccept): Future[ByteVector] = {
    val builder = DLCTxBuilder(offer, accept.withoutSigs)
    builder.buildFundingTx.map(_.txIdBE.bytes.xor(accept.tempContractId.bytes))
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

      newDLCDb = dlcDb.copy(contractIdOpt = Some(contractId))

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

  /** Updates the funding outpoint in the DLCDb corresponding to the given contractId */
  private def updateFundingOutPoint(
      contractId: ByteVector,
      outPoint: TransactionOutPoint): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with contractId ${contractId.toHex}"))
      }
      updated <- dlcDAO.update(dlcDb.updateFundingOutPoint(outPoint))
    } yield updated
  }

  /** Updates the closing txId in the DLCDb corresponding to the given contractId */
  private def updateClosingTxId(
      contractId: ByteVector,
      txId: DoubleSha256DigestBE): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with contractId ${contractId.toHex}"))
      }
      updated <- dlcDAO.update(dlcDb.copy(closingTxIdOpt = Some(txId)))
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

  /** Calculates the new state of the DLCDb based on the closing transaction */
  private def calculateAndSetState(dlcDb: DLCDb): Future[DLCDb] = {
    (dlcDb.contractIdOpt, dlcDb.closingTxIdOpt) match {
      case (Some(id), Some(txId)) =>
        executorAndSetupFromDb(id).map {
          case (_, setup) =>
            val state =
              if (txId == setup.refundTx.txIdBE) {
                DLCState.Refunded
              } else if (dlcDb.state == DLCState.Claimed) {
                DLCState.Claimed
              } else {
                DLCState.RemoteClaimed
              }

            dlcDb.copy(state = state)
        }
      case (None, None) | (None, Some(_)) | (Some(_), None) =>
        Future.successful(dlcDb)
    }
  }

  /** Process incoming utxos as normal, and then update the DLC states if applicable */
  override protected def processIncomingUtxos(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag]): Future[Vector[SpendingInfoDb]] = {
    super.processIncomingUtxos(tx, blockHashOpt, newTags).flatMap { res =>
      for {
        dlcDbs <- dlcDAO.findByFundingTxIds(Vector(tx.txIdBE))
        _ <-
          if (dlcDbs.nonEmpty) {
            insertTransaction(tx)
          } else FutureUtil.unit

        // Update the state to be confirmed or broadcasted
        updated = dlcDbs.map { dlcDb =>
          dlcDb.state match {
            case DLCState.Offered | DLCState.Accepted | DLCState.Signed |
                DLCState.Broadcasted =>
              if (blockHashOpt.isDefined)
                dlcDb.copy(state = DLCState.Confirmed)
              else dlcDb.copy(state = DLCState.Broadcasted)
            case DLCState.Confirmed | DLCState.Claimed |
                DLCState.RemoteClaimed | DLCState.Refunded =>
              dlcDb
          }
        }

        _ <- dlcDAO.updateAll(updated)
      } yield res
    }
  }

  override protected def processOutgoingUtxos(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[
    Vector[SpendingInfoDb]] = {
    super.processOutgoingUtxos(transaction, blockHashOpt).flatMap { res =>
      val outPoints = transaction.inputs.map(_.previousOutput).toVector

      for {
        dlcDbs <- dlcDAO.findByFundingOutPoints(outPoints)
        _ <-
          if (dlcDbs.nonEmpty) {
            insertTransaction(transaction)
          } else FutureUtil.unit

        withTx = dlcDbs.map(_.copy(closingTxIdOpt = Some(transaction.txIdBE)))
        updatedFs = withTx.map(calculateAndSetState)
        updated <- Future.sequence(updatedFs)

        _ <- dlcDAO.updateAll(updated)
      } yield res
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
      dlcOfferDbOpt <- dlcOfferDAO.findByParamHash(paramHash)
      dlcOffer <- dlcOfferDbOpt match {
        case Some(dlcOfferDb) =>
          logger.debug(
            s"DLC Offer (${paramHash.hex}) has already been made, returning offer")

          for {
            fundingInputs <-
              dlcInputsDAO.findByParamHash(paramHash, isInitiator = true)
            prevTxs <-
              transactionDAO.findByTxIdBEs(fundingInputs.map(_.outPoint.txIdBE))
          } yield {
            val inputRefs = fundingInputs.zip(prevTxs).map {
              case (input, tx) => input.toFundingInput(tx.transaction)
            }
            dlcOfferDb.toDLCOffer(inputRefs)
          }
        case None =>
          createNewDLCOffer(
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
      collateral: CurrencyUnit,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts): Future[DLCOffer] = {
    val paramHash = DLCMessage.calcParamHash(oracleInfo, contractInfo, timeouts)

    for {
      account <- getDefaultAccountForType(AddressType.SegWit)
      nextIndex <- getNextAvailableIndex(account, HDChainType.External)
      _ <- writeDLCKeysToAddressDb(account, nextIndex)

      (txBuilder, spendingInfos) <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = feeRate,
        fromAccount = account,
        fromTagOpt = None,
        markAsReserved = true
      )
      utxos = spendingInfos.map(DLCFundingInput.fromInputSigningInfo(_))

      changeSPK =
        txBuilder.finalizer.changeSPK
          .asInstanceOf[WitnessScriptPubKey]
      network = networkParameters.asInstanceOf[BitcoinNetwork]
      changeAddr = Bech32Address(changeSPK, network)

      dlcPubKeys = calcDLCPubKeys(account.xpub, nextIndex)

      _ = logger.debug(
        s"DLC Offer data collected, creating database entry, ${paramHash.hex}")

      offer = DLCOffer(contractInfo,
                       oracleInfo,
                       dlcPubKeys,
                       collateral.satoshis,
                       utxos,
                       changeAddr,
                       feeRate,
                       timeouts)

      dlcDb = DLCDb(
        paramHash = paramHash,
        tempContractId = offer.tempContractId,
        contractIdOpt = None,
        state = DLCState.Offered,
        isInitiator = true,
        account = account.hdAccount,
        keyIndex = nextIndex,
        oracleSigOpt = None,
        fundingOutPointOpt = None,
        fundingTxIdOpt = None,
        closingTxIdOpt = None
      )

      dlc <- dlcDAO.create(dlcDb)

      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(offer)

      dlcInputs = spendingInfos.map(funding =>
        DLCFundingInputDb(
          paramHash = dlc.paramHash,
          isInitiator = true,
          outPoint = funding.outPoint,
          output = funding.output,
          redeemScriptOpt = InputInfo.getRedeemScript(funding.inputInfo),
          witnessScriptOpt = InputInfo.getScriptWitness(funding.inputInfo)
        ))

      _ <- dlcInputsDAO.createAll(dlcInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
    } yield offer
  }

  private def initDLCForAccept(offer: DLCOffer): Future[(DLCDb, AccountDb)] = {
    dlcDAO.findByParamHash(offer.paramHash).flatMap {
      case Some(dlcDb) =>
        accountDAO
          .findByAccount(dlcDb.account)
          .map(account => (dlcDb, account.get))
      case None =>
        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          nextIndex <- getNextAvailableIndex(account, HDChainType.External)
          dlc = {
            DLCDb(
              paramHash = offer.paramHash,
              tempContractId = offer.tempContractId,
              contractIdOpt = None,
              state = DLCState.Accepted,
              isInitiator = false,
              account = account.hdAccount,
              keyIndex = nextIndex,
              oracleSigOpt = None,
              fundingOutPointOpt = None,
              fundingTxIdOpt = None,
              closingTxIdOpt = None
            )
          }
          _ <- writeDLCKeysToAddressDb(account, nextIndex)
          writtenDLC <- dlcDAO.create(dlc)
        } yield (writtenDLC, account)
    }
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
      (dlc, account) <- initDLCForAccept(offer)
      dlcAcceptDbOpt <- dlcAcceptDAO.findByParamHash(paramHash)
      dlcAccept <- dlcAcceptDbOpt match {
        case Some(dlcAcceptDb) =>
          logger.debug(
            s"DLC Accept (${paramHash.hex}) has already been made, returning accept")
          for {
            fundingInputs <-
              dlcInputsDAO.findByParamHash(paramHash, isInitiator = false)
            prevTxs <-
              transactionDAO.findByTxIdBEs(fundingInputs.map(_.outPoint.txIdBE))
            outcomeSigDbs <- dlcSigsDAO.findByParamHash(paramHash)
            refundSigDb <- dlcRefundSigDAO.read(paramHash, false)
          } yield {
            val inputRefs = fundingInputs.zip(prevTxs).map {
              case (input, tx) => input.toFundingInput(tx.transaction)
            }
            val outcomeSigs = outcomeSigDbs.map(_.toTuple)

            dlcAcceptDb.toDLCAccept(inputRefs,
                                    outcomeSigs,
                                    refundSigDb.get.refundSig)
          }
        case None =>
          createNewDLCAccept(dlc, account, collateral, offer)
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
        fromTagOpt = None,
        markAsReserved = true
      )
      network = networkParameters.asInstanceOf[BitcoinNetwork]

      utxos = spendingInfos.map(DLCFundingInput.fromInputSigningInfo(_))

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

      fundingBuilder = builder.fundingTxBuilder
      spkDb = ScriptPubKeyDb(fundingBuilder.fundingSPK)

      _ <- scriptPubKeyDAO.create(spkDb)

      cetSigs <- signer.createCETSigs()

      _ = logger.debug(
        s"DLC Accept data collected, creating database entry, ${dlc.paramHash.hex}")

      dlcAcceptDb = DLCAcceptDb(
        paramHash = dlc.paramHash,
        tempContractId = offer.tempContractId,
        fundingKey = dlcPubKeys.fundingKey,
        finalAddress = dlcPubKeys.payoutAddress,
        totalCollateral = collateral,
        changeAddress = changeAddr
      )

      sigsDbs = cetSigs.outcomeSigs.map(sig =>
        DLCCETSignatureDb(dlc.paramHash, isInitiator = false, sig._1, sig._2))

      refundSigDb =
        DLCRefundSigDb(dlc.paramHash, isInitiator = false, cetSigs.refundSig)

      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(offer)

      offerInputs = offer.fundingInputs.map(funding =>
        DLCFundingInputDb(
          paramHash = dlc.paramHash,
          isInitiator = true,
          outPoint = funding.outPoint,
          output = funding.output,
          redeemScriptOpt = funding.redeemScriptOpt,
          witnessScriptOpt = None
        ))

      offerPrevTxs = offer.fundingInputs.map(funding =>
        TransactionDbHelper.fromTransaction(funding.prevTx))

      acceptInputs = spendingInfos.map(utxo =>
        DLCFundingInputDb(
          paramHash = dlc.paramHash,
          isInitiator = false,
          outPoint = utxo.outPoint,
          output = utxo.output,
          redeemScriptOpt = InputInfo.getRedeemScript(utxo.inputInfo),
          witnessScriptOpt = InputInfo.getScriptWitness(utxo.inputInfo)
        ))

      accept =
        dlcAcceptDb.toDLCAccept(utxos, cetSigs.outcomeSigs, cetSigs.refundSig)

      _ = require(accept.tempContractId == offer.tempContractId,
                  "Offer and Accept have differing tempContractIds!")

      _ <- remoteTxDAO.upsertAll(offerPrevTxs)
      _ <- dlcInputsDAO.createAll(offerInputs ++ acceptInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
      _ <- dlcAcceptDAO.create(dlcAcceptDb)
      _ <- dlcSigsDAO.createAll(sigsDbs)
      _ <- dlcRefundSigDAO.create(refundSigDb)
      dlcDb <- updateDLCContractIds(offer, accept)

      fundingTx <- builder.buildFundingTx
      outPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
      _ <- updateFundingOutPoint(dlcDb.contractIdOpt.get, outPoint)
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
        val acceptInputs = accept.fundingInputs.map(funding =>
          DLCFundingInputDb(
            paramHash = paramHash,
            isInitiator = false,
            outPoint = funding.outPoint,
            output = funding.output,
            redeemScriptOpt = funding.redeemScriptOpt,
            witnessScriptOpt = None
          ))

        val acceptPrevTxs = accept.fundingInputs.map { funding =>
          TransactionDbHelper.fromTransaction(funding.prevTx)
        }

        val sigsDbs = accept.cetSigs.outcomeSigs
          .map(sig =>
            DLCCETSignatureDb(paramHash, isInitiator = false, sig._1, sig._2))
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
          _ <- remoteTxDAO.upsertAll(acceptPrevTxs)
          _ <- dlcInputsDAO.upsertAll(acceptInputs)
          _ <- dlcSigsDAO.upsertAll(sigsDbs)
          _ <- dlcRefundSigDAO.upsert(refundSigDb)
          _ <- dlcAcceptDAO.upsert(dlcAcceptDb)
          _ <- dlcDAO.update(dlc.updateState(DLCState.Accepted))

          // .get is safe here because we must have an offer if we have a dlcDAO
          offerDb <- dlcOfferDAO.findByParamHash(dlc.paramHash).map(_.get)
          offerInputs <-
            dlcInputsDAO.findByParamHash(dlc.paramHash, isInitiator = true)
          prevTxs <-
            transactionDAO.findByTxIdBEs(offerInputs.map(_.outPoint.txIdBE))
          offer = offerDb.toDLCOffer(offerInputs.zip(prevTxs).map {
            case (input, tx) => input.toFundingInput(tx.transaction)
          })

          dlcDb <- updateDLCContractIds(offer, accept)

          builder = DLCTxBuilder(offer, accept.withoutSigs)
          fundingTx <- builder.buildFundingTx
          outPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
          spkDb = ScriptPubKeyDb(builder.fundingTxBuilder.fundingSPK)
          _ <- scriptPubKeyDAO.create(spkDb)
          updatedDLCDb <-
            updateFundingOutPoint(dlcDb.contractIdOpt.get, outPoint)
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

      spk = signer.builder.fundingTxBuilder.fundingSPK

      cetSigs <- signer.createCETSigs()
      fundingSigs <- signer.createFundingTxSigs()

      refundSigDb =
        DLCRefundSigDb(dlc.paramHash, isInitiator = true, cetSigs.refundSig)
      _ <- dlcRefundSigDAO.upsert(refundSigDb)

      sigDbs = cetSigs.outcomeSigs.map(sig =>
        DLCCETSignatureDb(dlc.paramHash, isInitiator = true, sig._1, sig._2))
      _ <- dlcSigsDAO.upsertAll(sigDbs.toVector)

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

  def verifyCETSigs(sign: DLCSign): Future[Boolean] = {
    verifierFromDb(sign.contractId).map { verifier =>
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

  def verifyRefundSig(sign: DLCSign): Future[Boolean] = {
    verifierFromDb(sign.contractId).map { verifier =>
      verifier.verifyRefundSig(sign.cetSigs.refundSig)
    }
  }

  def verifyFundingSigs(
      inputs: Vector[DLCFundingInputDb],
      sign: DLCSign): Future[Boolean] = {
    if (inputs.count(!_.isInitiator) == sign.fundingSigs.length) {
      verifierFromDb(sign.contractId).map { verifier =>
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
      isValid <- verifyFundingSigs(inputs = inputs, sign = sign)
      _ <- {
        if (!isValid)
          Future.failed(new IllegalArgumentException(
            s"Funding Signatures provided are not valid! got ${sign.fundingSigs}"))
        else FutureUtil.unit
      }

      updatedInputs = sign.fundingSigs.map {
        case (outPoint, witness) =>
          inputs.find(_.outPoint == outPoint) match {
            case Some(inputDb) =>
              inputDb.copy(witnessScriptOpt = Some(witness))
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
          .map(sig =>
            DLCCETSignatureDb(dlc.paramHash,
                              isInitiator = true,
                              sig._1,
                              sig._2))
          .toVector

        for {
          isRefundSigValid <- verifyRefundSig(sign)
          _ = if (!isRefundSigValid)
            throw new IllegalArgumentException(
              s"Refund sig provided is not valid! got ${sign.cetSigs.refundSig}")

          isCETSigsValid <- verifyCETSigs(sign)
          _ = if (!isCETSigsValid)
            throw new IllegalArgumentException(
              s"CET sigs provided are not valid! got ${sign.cetSigs.outcomeSigs}")

          _ <- addFundingSigs(sign)
          _ <- dlcSigsDAO.createAll(sigsDbs)
          _ <- dlcRefundSigDAO.create(refundSigDb)
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
      localFundingInputs = fundingInputsDb.filter(_.isInitiator)
      prevTxs <-
        transactionDAO.findByTxIdBEs(localFundingInputs.map(_.outPoint.txIdBE))
    } yield {
      val offerFundingInputs =
        localFundingInputs.zip(prevTxs).map {
          case (input, tx) => input.toFundingInput(tx.transaction)
        }

      val builder =
        DLCTxBuilder(dlcOffer.toDLCOffer(offerFundingInputs),
                     accept.withoutSigs)

      DLCSignatureVerifier(builder, dlcDb.isInitiator)
    }
  }

  private def verifierFromDb(
      contractId: ByteVector): Future[DLCSignatureVerifier] = {
    getAllDLCData(contractId).flatMap {
      case (dlcDb, dlcOffer, dlcAccept, _, fundingInputsDb, _) =>
        verifierFromDbData(dlcDb, dlcOffer, dlcAccept, fundingInputsDb)
    }
  }

  private def builderFromDbData(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb]): Future[DLCTxBuilder] = {
    val (offerDbFundingInputs, acceptDbFundingInputs) =
      fundingInputsDb.partition(_.isInitiator)
    val (localDbFundingInputs, remoteDbFundingInputs) = if (dlcDb.isInitiator) {
      (offerDbFundingInputs, acceptDbFundingInputs)
    } else {
      (acceptDbFundingInputs, offerDbFundingInputs)
    }

    for {
      localPrevTxs <- transactionDAO.findByTxIdBEs(
        localDbFundingInputs.map(_.outPoint.txIdBE))
      remotePrevTxs <-
        remoteTxDAO.findByTxIdBEs(remoteDbFundingInputs.map(_.outPoint.txIdBE))
    } yield {
      val localFundingInputs = localDbFundingInputs.zip(localPrevTxs).map {
        case (input, tx) => input.toFundingInput(tx.transaction)
      }
      val remoteFundingInputs = remoteDbFundingInputs.zip(remotePrevTxs).map {
        case (input, tx) => input.toFundingInput(tx.transaction)
      }

      val (offerFundingInputs, acceptFundingInputs) = if (dlcDb.isInitiator) {
        (localFundingInputs, remoteFundingInputs)
      } else {
        (remoteFundingInputs, localFundingInputs)
      }

      val offer = dlcOffer.toDLCOffer(offerFundingInputs)
      val accept = dlcAccept.toDLCAcceptWithoutSigs(offer.tempContractId,
                                                    acceptFundingInputs)

      DLCTxBuilder(offer, accept)
    }
  }

  private def verifierFromDbData(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb]): Future[
    DLCSignatureVerifier] = {
    val builderF =
      builderFromDbData(dlcDb, dlcOffer, dlcAccept, fundingInputsDb)

    builderF.map(DLCSignatureVerifier(_, dlcDb.isInitiator))
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
    for {
      fundingUtxos <- fundingUtxosFromDb(dlcDb, fundingInputsDb)
      builder <- builderFromDbData(dlcDb, dlcOffer, dlcAccept, fundingInputsDb)
    } yield {
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
        // Filter for only counter party's outcome sigs
        val outcomeSigs = outcomeSigDbs
          .filter(_.isInitiator == !dlcDb.isInitiator)
          .map(_.toTuple)

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
              .map { input =>
                input.witnessScriptOpt match {
                  case Some(witnessScript) =>
                    witnessScript match {
                      case EmptyScriptWitness =>
                        throw new RuntimeException(
                          "Script witness cannot be empty")
                      case witness: ScriptWitnessV0 => (input.outPoint, witness)
                    }
                  case None => throw new RuntimeException("")
                }
              }
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
            .map { input =>
              input.witnessScriptOpt match {
                case Some(witnessScript) =>
                  witnessScript match {
                    case EmptyScriptWitness =>
                      throw new RuntimeException(
                        "Script witness cannot be empty")
                    case witness: ScriptWitnessV0 => (input.outPoint, witness)
                  }
                case None => throw new RuntimeException("")
              }
            }
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

      _ <- processTransaction(outcome.cet, None)
      _ <- updateDLCState(contractId, DLCState.Claimed)
      _ <- updateClosingTxId(contractId, outcome.cet.txIdBE)
    } yield {
      outcome.cet
    }
  }

  override def executeDLCRefund(contractId: ByteVector): Future[Transaction] = {
    for {
      (executor, setup) <- executorAndSetupFromDb(contractId)
      outcome = executor.executeRefundDLC(setup)
      _ <- processTransaction(outcome.refundTx, blockHashOpt = None)
      _ <- updateDLCState(contractId, DLCState.Refunded)
      _ <- updateClosingTxId(contractId, outcome.refundTx.txIdBE)
    } yield {
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
