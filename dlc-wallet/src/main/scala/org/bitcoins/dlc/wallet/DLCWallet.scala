package org.bitcoins.dlc.wallet

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.DLCState._
import org.bitcoins.commons.jsonmodels.dlc.SerializedDLCStatus._
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
import org.bitcoins.core.protocol.tlv._
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
    builder.calcContractId
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
      _ = logger.debug(s"Updating DLC contract Ids")
      updated <- dlcDAO.update(newDLCDb)
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
      _ = logger.debug(s"Updating DLC (${contractId.toHex}) to state $state")
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
      _ = logger.debug(
        s"Updating DLC (${contractId.toHex}) funding outpoint to ${outPoint.hex}")
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
      _ = logger.debug(
        s"Updating DLC (${contractId.toHex}) closing txId to ${txId.hex}")
      updated <- dlcDAO.update(dlcDb.copy(closingTxIdOpt = Some(txId)))
    } yield updated
  }

  private def updateDLCOracleSigs(
      contractId: ByteVector,
      sigs: Vector[SchnorrDigitalSignature]): Future[DLCDb] = {
    dlcDAO.findByContractId(contractId).flatMap {
      case Some(dlcDb) =>
        logger.debug(
          s"Updating DLC's (${contractId.toHex}) oracle sigs to ${sigs.map(_.hex)}")
        dlcDAO.update(dlcDb.copy(oracleSigsOpt = Some(sigs)))
      case None =>
        Future.failed(
          new NoSuchElementException(
            s"No DLC found with that contractId ${contractId.toHex}"))
    }
  }

  private def updateDLCOutcome(
      contractId: ByteVector,
      outcome: DLCOutcomeType): Future[DLCDb] = {
    dlcDAO.findByContractId(contractId).flatMap {
      case Some(dlcDb) =>
        logger.debug(
          s"Updating DLC's (${contractId.toHex}) outcome to $outcome")
        dlcDAO.update(dlcDb.copy(outcomeOpt = Some(outcome)))
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
      logger.debug(s"Wrote DLC key addresses to database using index $index")
      Vector(zero, one)
    }
  }

  /** Calculates the new state of the DLCDb based on the closing transaction */
  private def calculateAndSetState(dlcDb: DLCDb): Future[DLCDb] = {
    (dlcDb.contractIdOpt, dlcDb.closingTxIdOpt) match {
      case (Some(id), Some(txId)) =>
        executorAndSetupFromDb(id).flatMap {
          case (_, setup) =>
            if (txId == setup.refundTx.txIdBE) {
              Future.successful(dlcDb.copy(state = DLCState.Refunded))
            } else if (dlcDb.state == DLCState.Claimed) {
              Future.successful(dlcDb.copy(state = DLCState.Claimed))
            } else {
              val withState = dlcDb.copy(state = DLCState.RemoteClaimed)
              if (dlcDb.outcomeOpt.isEmpty || dlcDb.oracleSigsOpt.isEmpty) {
                calculateAndSetOutcome(withState)
              } else Future.successful(withState)
            }

        }
      case (None, None) | (None, Some(_)) | (Some(_), None) =>
        Future.successful(dlcDb)
    }
  }

  private def calculateAndSetOutcome(dlcDb: DLCDb): Future[DLCDb] = {
    if (dlcDb.state == DLCState.RemoteClaimed && dlcDb.outcomeOpt.isEmpty) {
      findDLC(dlcDb.paramHash).map {
        case Some(status: DLCStatus.RemoteClaimed) =>
          dlcDb.copy(outcomeOpt = Some(status.outcome),
                     oracleSigsOpt = Some(Vector(status.oracleSig)))
        case res @ (None | Some(_)) =>
          throw new RuntimeException(s"Unexpected return from findDLC got $res")
      }
    } else {
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
            logger.info(
              s"Processing tx ${tx.txIdBE.hex} for ${dlcDbs.size} DLC(s)")
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
            logger.info(
              s"Processing tx ${transaction.txIdBE.hex} for ${dlcDbs.size} DLC(s)")
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
            val inputRefs = matchPrevTxsWithInputs(fundingInputs, prevTxs)
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
    logger.info("Creating DLC Offer")
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

      offer = DLCOffer(OracleAndContractInfo(oracleInfo, contractInfo),
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
        oracleSigsOpt = None,
        fundingOutPointOpt = None,
        fundingTxIdOpt = None,
        closingTxIdOpt = None,
        outcomeOpt = None
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

      _ = logger.info(
        s"Created offer with tempContractId ${offer.tempContractId.hex}")
      _ <- dlcInputsDAO.createAll(dlcInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
    } yield offer
  }

  private def initDLCForAccept(offer: DLCOffer): Future[(DLCDb, AccountDb)] = {
    logger.info(
      s"Initializing DLC from received offer with tempContractId ${offer.tempContractId.hex}")
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
              oracleSigsOpt = None,
              fundingOutPointOpt = None,
              fundingTxIdOpt = None,
              closingTxIdOpt = None,
              outcomeOpt = None
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

    val collateral = offer.contractInfo.max - offer.totalCollateral

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
            val inputRefs = matchPrevTxsWithInputs(fundingInputs, prevTxs)
            val outcomeSigs = outcomeSigDbs.map(_.toTuple)

            dlcAcceptDb.toDLCAccept(inputRefs,
                                    outcomeSigs.map {
                                      case (outcome, sig) => outcome -> sig
                                    },
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
    logger.info(
      s"Creating DLC Accept for tempContractId ${offer.tempContractId.hex}")
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

      contractId <- builder.calcContractId

      signer = DLCTxSigner(builder = builder,
                           isInitiator = false,
                           fundingKey = fundingPrivKey,
                           finalAddress = dlcPubKeys.payoutAddress,
                           fundingUtxos = spendingInfos)

      fundingBuilder = builder.fundingTxBuilder
      spkDb = ScriptPubKeyDb(fundingBuilder.fundingSPK)

      _ <- scriptPubKeyDAO.create(spkDb)

      _ = logger.info(s"Creating CET Sigs for ${contractId.toHex}")
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

      _ = logger.info(
        s"Created DLCAccept for tempContractId ${offer.tempContractId.hex} with contract Id ${contractId.toHex}")

      fundingTx <- builder.buildFundingTx
      outPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
      _ <- updateFundingOutPoint(dlcDb.contractIdOpt.get, outPoint)
    } yield accept
  }

  def registerDLCAccept(accept: DLCAccept): Future[DLCDb] = {
    logger.debug(
      s"Checking if DLC Accept with tempContractId ${accept.tempContractId.hex} has already been registered")
    val dbsF = for {
      dlcDbOpt <- dlcDAO.findByTempContractId(accept.tempContractId)
      (dlcDb, acceptDbOpt) <- dlcDbOpt match {
        case Some(db) =>
          dlcAcceptDAO
            .findByParamHash(db.paramHash)
            .map(acceptDbOpt => (db, acceptDbOpt))
        case None =>
          Future.failed(new RuntimeException(
            s"No DLC Offer found with corresponding tempContractId ${accept.tempContractId.hex}, this wallet did not create the corresponding offer"))
      }
    } yield (dlcDb, acceptDbOpt)

    dbsF.flatMap {
      case (dlc, None) =>
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

        val refundSigDb = DLCRefundSigDb(paramHash,
                                         isInitiator = false,
                                         accept.cetSigs.refundSig)

        logger.info(
          s"Verifying ${accept.cetSigs.outcomeSigs.size} CET Signatures")
        for {
          isCETSigsValid <- verifyCETSigs(accept)
          _ = if (!isCETSigsValid)
            throw new IllegalArgumentException(
              s"CET sigs provided are not valid! got ${accept.cetSigs.outcomeSigs}")
          isRefundSigValid <- verifyRefundSig(accept)
          _ = if (!isRefundSigValid)
            throw new IllegalArgumentException(
              s"Refund sig provided is not valid! got ${accept.cetSigs.refundSig}")

          _ = logger.debug(
            s"CET Signatures for tempContractId ${accept.tempContractId.hex} were valid, adding to database")

          _ <- remoteTxDAO.createAll(acceptPrevTxs)
          _ <- dlcInputsDAO.createAll(acceptInputs)
          _ <- dlcSigsDAO.createAll(sigsDbs)
          _ <- dlcRefundSigDAO.upsert(refundSigDb)
          _ <- dlcAcceptDAO.upsert(dlcAcceptDb)
          _ <- dlcDAO.update(dlc.updateState(DLCState.Accepted))

          // .get is safe here because we must have an offer if we have a dlcDAO
          offerDb <- dlcOfferDAO.findByParamHash(dlc.paramHash).map(_.get)
          offerInputs <-
            dlcInputsDAO.findByParamHash(dlc.paramHash, isInitiator = true)
          prevTxs <-
            transactionDAO.findByTxIdBEs(offerInputs.map(_.outPoint.txIdBE))
          offer =
            offerDb.toDLCOffer(matchPrevTxsWithInputs(offerInputs, prevTxs))

          dlcDb <- updateDLCContractIds(offer, accept)

          builder = DLCTxBuilder(offer, accept.withoutSigs)
          fundingTx <- builder.buildFundingTx
          outPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
          spkDb = ScriptPubKeyDb(builder.fundingTxBuilder.fundingSPK)
          _ <- scriptPubKeyDAO.create(spkDb)
          updatedDLCDb <-
            updateFundingOutPoint(dlcDb.contractIdOpt.get, outPoint)
        } yield updatedDLCDb
      case (dlc, Some(_)) =>
        logger.debug(
          s"DLC Accept (${dlc.contractIdOpt.get.toHex}) has already been registered")
        Future.successful(dlc)
    }
  }

  override def signDLC(acceptTLV: DLCAcceptTLV): Future[DLCSign] = {
    val tempId = acceptTLV.tempContractId

    for {
      dlcDbOpt <- dlcDAO.findByTempContractId(tempId)
      dlcDb <- dlcDbOpt match {
        case Some(db) => Future.successful(db)
        case None =>
          Future.failed(new RuntimeException(
            s"No DLC found with corresponding tempContractId ${tempId.hex}, this wallet did not create the corresponding offer"))
      }
      offerDbOpt <- dlcOfferDAO.findByParamHash(dlcDb.paramHash)
      // .get should be safe now
      offerDb = offerDbOpt.get
      accept =
        DLCAccept.fromTLV(acceptTLV, walletConfig.network, offerDb.contractInfo)
      dlcSign <- signDLC(accept)
    } yield dlcSign
  }

  /**
    * Creates signatures for the DLCs CETs and Funding Inputs
    *
    * This is the second step of the initiator
    */
  override def signDLC(accept: DLCAccept): Future[DLCSign] = {
    for {
      dlc <- registerDLCAccept(accept)
      // .get should be safe now
      contractId = dlc.contractIdOpt.get

      signer <- signerFromDb(dlc.paramHash)

      mySigs <- dlcSigsDAO.findByParamHash(dlc.paramHash, isInit = true)

      cetSigs <-
        if (mySigs.isEmpty) {
          logger.info(s"Creating CET Sigs for contract ${contractId.toHex}")
          for {
            sigs <- signer.createCETSigs()
            sigDbs = sigs.outcomeSigs.map(sig =>
              DLCCETSignatureDb(dlc.paramHash,
                                isInitiator = true,
                                sig._1,
                                sig._2))
            _ <- dlcSigsDAO.createAll(sigDbs)
          } yield sigs
        } else {
          logger.debug(s"CET Sigs already created for ${contractId.toHex}")
          val outcomeSigs = mySigs.map(_.toTuple)
          dlcRefundSigDAO
            .findByParamHash(dlc.paramHash, isInit = true)
            .flatMap {
              case Some(refundDb) =>
                Future.successful(
                  CETSignatures(outcomeSigs, refundDb.refundSig))
              case None =>
                signer.createRefundSig().map { sig =>
                  CETSignatures(outcomeSigs, sig)
                }
            }
        }

      _ = logger.info(s"Creating funding sigs for ${contractId.toHex}")
      fundingSigs <- signer.createFundingTxSigs()

      refundSigDb =
        DLCRefundSigDb(dlc.paramHash, isInitiator = true, cetSigs.refundSig)
      _ <- dlcRefundSigDAO.upsert(refundSigDb)

      _ <- updateDLCState(dlc.contractIdOpt.get, DLCState.Signed)
    } yield DLCSign(cetSigs, fundingSigs, contractId)
  }

  def verifyCETSigs(accept: DLCAccept): Future[Boolean] = {
    verifierFromAccept(accept).flatMap(
      _.verifyCETSigs(accept.cetSigs.outcomeSigs))
  }

  def verifyCETSigs(sign: DLCSign): Future[Boolean] = {
    verifierFromDb(sign.contractId).flatMap(
      _.verifyCETSigs(sign.cetSigs.outcomeSigs))
  }

  def verifyRefundSig(accept: DLCAccept): Future[Boolean] = {
    verifierFromAccept(accept).map(_.verifyRefundSig(accept.cetSigs.refundSig))
  }

  def verifyRefundSig(sign: DLCSign): Future[Boolean] = {
    verifierFromDb(sign.contractId).map(
      _.verifyRefundSig(sign.cetSigs.refundSig))
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

      _ = logger.info(
        s"Verifying ${sign.fundingSigs.length} funding sigs for contract ${sign.contractId.toHex}")
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

  override def addDLCSigs(signTLV: DLCSignTLV): Future[DLCDb] = {
    val contractId = signTLV.contractId

    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb <- dlcDbOpt match {
        case Some(db) => Future.successful(db)
        case None =>
          Future.failed(new RuntimeException(
            s"No DLC found with corresponding contractId ${contractId.toHex}"))
      }
      offerDbOpt <- dlcOfferDAO.findByParamHash(dlcDb.paramHash)
      // .get should be safe now
      offerDb = offerDbOpt.get
      fundingInputDbs <- dlcInputsDAO.findByParamHash(dlcDb.paramHash)

      txIds = fundingInputDbs.map(_.outPoint.txIdBE)
      remotePrevTxs <- remoteTxDAO.findByTxIdBEs(txIds)
      localPrevTxs <- transactionDAO.findByTxIdBEs(txIds)

      prevTxs = (remotePrevTxs ++ localPrevTxs).map(_.transaction)
      txs = prevTxs.groupBy(_.txIdBE)

      fundingInputs = fundingInputDbs.map(input =>
        input.toFundingInput(txs(input.outPoint.txIdBE).head))

      offer = offerDb.toDLCOffer(fundingInputs)

      sign = DLCSign.fromTLV(signTLV, offer)
      result <- addDLCSigs(sign)
    } yield result
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

        logger.info(
          s"Verifying CET Signatures for contract ${sign.contractId.toHex}")
        for {
          isRefundSigValid <- verifyRefundSig(sign)
          _ = if (!isRefundSigValid)
            throw new IllegalArgumentException(
              s"Refund sig provided is not valid! got ${sign.cetSigs.refundSig}")

          isCETSigsValid <- verifyCETSigs(sign)
          _ = if (!isCETSigsValid)
            throw new IllegalArgumentException(
              s"CET sigs provided are not valid! got ${sign.cetSigs.outcomeSigs}")

          _ = logger.info(
            s"CET Signatures are valid for contract ${sign.contractId.toHex}")

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

  private def getDLCFundingData(contractId: ByteVector): Future[
    (DLCDb, DLCOfferDb, DLCAcceptDb, Vector[DLCFundingInputDb])] = {
    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb = dlcDbOpt.get
      (dlcDb, dlcOffer, dlcAccept, fundingInputs) <- getDLCFundingData(
        dlcDb.paramHash)
    } yield (dlcDb, dlcOffer, dlcAccept, fundingInputs)
  }

  private def getDLCFundingData(paramHash: Sha256DigestBE): Future[
    (DLCDb, DLCOfferDb, DLCAcceptDb, Vector[DLCFundingInputDb])] = {
    for {
      dlcDbOpt <- dlcDAO.findByParamHash(paramHash)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByParamHash(paramHash)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByParamHash(paramHash)
      dlcAccept = dlcAcceptOpt.get
      fundingInputs <- dlcInputsDAO.findByParamHash(paramHash)
    } yield (dlcDb, dlcOffer, dlcAccept, fundingInputs)
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
      (dlcDb, dlcOffer, dlcAccept, fundingInputs) <- getDLCFundingData(
        paramHash)
      refundSigs <- dlcRefundSigDAO.findByParamHash(paramHash)
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
        matchPrevTxsWithInputs(localFundingInputs, prevTxs)

      val builder =
        DLCTxBuilder(dlcOffer.toDLCOffer(offerFundingInputs),
                     accept.withoutSigs)

      DLCSignatureVerifier(builder, dlcDb.isInitiator)
    }
  }

  private def verifierFromDb(
      contractId: ByteVector): Future[DLCSignatureVerifier] = {
    getDLCFundingData(contractId).flatMap {
      case (dlcDb, dlcOffer, dlcAccept, fundingInputsDb) =>
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
      val localFundingInputs = matchPrevTxsWithInputs(inputs =
                                                        localDbFundingInputs,
                                                      prevTxs = localPrevTxs)

      val remoteFundingInputs = matchPrevTxsWithInputs(inputs =
                                                         remoteDbFundingInputs,
                                                       prevTxs = remotePrevTxs)

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

  /** Takes in a list of inputs to fund DLCs, and pairs them with the full funding transaction for this input
    * and then converts the input tx pair to a [[DLCFundingInput]]
    * @throws NoSuchElementException when we have an input we cannot find the funding transaction for
    */
  private def matchPrevTxsWithInputs(
      inputs: Vector[DLCFundingInputDb],
      prevTxs: Vector[TransactionDb]): Vector[DLCFundingInput] = {
    inputs.map { i =>
      prevTxs.find(_.txId == i.outPoint.txId) match {
        case Some(txDb) => i.toFundingInput(txDb.transaction)
        case None =>
          throw new NoSuchElementException(
            s"Could not find previous transaction with txIdBE=${i.outPoint.txId.flip.hex}")
      }
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
      (dlcDb, dlcOffer, dlcAccept, fundingInputsDb) <- getDLCFundingData(
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
      builder <- builderFromDbData(dlcDb = dlcDb,
                                   dlcOffer = dlcOffer,
                                   dlcAccept = dlcAccept,
                                   fundingInputsDb = fundingInputsDb)
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
          .map { case (outcome, sig) => outcome -> sig }

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
      (dlcDb, dlcOffer, dlcAccept, fundingInputs) <- getDLCFundingData(
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
      _ = logger.info(
        s"Created funding transaction ${fundingTx.txIdBE.hex} for contract ${contractId.toHex}")
    } yield fundingTx
  }

  override def broadcastDLCFundingTx(
      contractId: ByteVector): Future[Transaction] = {
    for {
      tx <- getDLCFundingTx(contractId)
      _ = logger.info(
        s"Broadcasting funding transaction ${tx.txIdBE.hex} for contract ${contractId.toHex}")
      _ <- broadcastTransaction(tx)

      _ <- updateDLCState(contractId, DLCState.Broadcasted)
    } yield tx
  }

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Vector[SchnorrDigitalSignature]): Future[Transaction] = {
    require(oracleSigs.nonEmpty, "Must provide at least one oracle signature")
    for {
      (executor, setup) <- executorAndSetupFromDb(contractId)

      payout = executor.getPayout(oracleSigs)
      _ = if (payout <= 0.satoshis)
        throw new UnsupportedOperationException(
          "Cannot execute a losing outcome")

      executed <- executor.executeDLC(setup, oracleSigs)
      (tx, outcome) = (executed.cet, executed.outcome)
      _ = logger.info(
        s"Created DLC execution transaction ${tx.txIdBE.hex} for contract ${contractId.toHex}")

      _ <- updateDLCOracleSigs(contractId, oracleSigs)

      _ <- processTransaction(tx, None)
      _ <- updateDLCState(contractId, DLCState.Claimed)
      _ <- updateDLCOutcome(contractId, outcome)
      _ <- updateClosingTxId(contractId, tx.txIdBE)
    } yield tx
  }

  override def executeDLCRefund(contractId: ByteVector): Future[Transaction] = {
    for {
      (executor, setup) <- executorAndSetupFromDb(contractId)
      refundTx = executor.executeRefundDLC(setup).refundTx
      _ = logger.info(
        s"Created DLC refund transaction ${refundTx.txIdBE.hex} for contract ${contractId.toHex}")
      _ <- processTransaction(refundTx, blockHashOpt = None)
      _ <- updateDLCState(contractId, DLCState.Refunded)
      _ <- updateClosingTxId(contractId, refundTx.txIdBE)
    } yield refundTx
  }

  override def listDLCs(): Future[Vector[SerializedDLCStatus]] = {
    for {
      ids <- dlcDAO.findAll().map(_.map(_.paramHash))
      dlcFs = ids.map(findSerializedDLC)
      dlcs <- Future.sequence(dlcFs)
    } yield {
      dlcs.collect {
        case Some(dlc) => dlc
      }
    }
  }

  def findSerializedDLC(
      paramHash: Sha256DigestBE): Future[Option[SerializedDLCStatus]] = {
    for {
      dlcDbOpt <- dlcDAO.read(paramHash)
      offerDbOpt <- dlcOfferDAO.read(paramHash)
    } yield (dlcDbOpt, offerDbOpt) match {
      case (Some(dlcDb), Some(offerDb)) =>
        val totalCollateral = offerDb.contractInfo.max

        val localCollateral = if (dlcDb.isInitiator) {
          offerDb.totalCollateral
        } else {
          totalCollateral - offerDb.totalCollateral
        }

        val serializedStatus = dlcDb.state match {
          case DLCState.Offered =>
            SerializedOffered(
              paramHash,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              offerDb.oracleInfo,
              offerDb.contractInfo,
              offerDb.dlcTimeouts,
              offerDb.feeRate,
              totalCollateral,
              localCollateral
            )
          case DLCState.Accepted =>
            SerializedAccepted(
              paramHash,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              offerDb.oracleInfo,
              offerDb.contractInfo,
              offerDb.dlcTimeouts,
              offerDb.feeRate,
              totalCollateral,
              localCollateral
            )
          case DLCState.Signed =>
            SerializedSigned(
              paramHash,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              offerDb.oracleInfo,
              offerDb.contractInfo,
              offerDb.dlcTimeouts,
              offerDb.feeRate,
              totalCollateral,
              localCollateral
            )
          case DLCState.Broadcasted =>
            SerializedBroadcasted(
              paramHash,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              offerDb.oracleInfo,
              offerDb.contractInfo,
              offerDb.dlcTimeouts,
              offerDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get
            )
          case DLCState.Confirmed =>
            SerializedConfirmed(
              paramHash,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              offerDb.oracleInfo,
              offerDb.contractInfo,
              offerDb.dlcTimeouts,
              offerDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get
            )
          case DLCState.Claimed =>
            SerializedClaimed(
              paramHash,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              offerDb.oracleInfo,
              offerDb.contractInfo,
              offerDb.dlcTimeouts,
              offerDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get,
              dlcDb.closingTxIdOpt.get,
              dlcDb.oracleSigsOpt.get,
              dlcDb.outcomeOpt.get
            )
          case DLCState.RemoteClaimed =>
            val oracleSigs = dlcDb.oracleSigsOpt.get
            require(oracleSigs.size == 1,
                    "Remote claimed should only have one oracle sig")

            SerializedRemoteClaimed(
              paramHash,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              offerDb.oracleInfo,
              offerDb.contractInfo,
              offerDb.dlcTimeouts,
              offerDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get,
              dlcDb.closingTxIdOpt.get,
              oracleSigs.head,
              dlcDb.outcomeOpt.get
            )
          case DLCState.Refunded =>
            SerializedRefunded(
              paramHash,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              offerDb.oracleInfo,
              offerDb.contractInfo,
              offerDb.dlcTimeouts,
              offerDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get,
              dlcDb.closingTxIdOpt.get
            )
        }

        Some(serializedStatus)
      case (None, None) | (None, Some(_)) | (Some(_), None) =>
        None
    }
  }

  def findDLC(paramHash: Sha256DigestBE): Future[Option[DLCStatus]] = {
    dlcDAO.read(paramHash).flatMap {
      case None => FutureUtil.none
      case Some(dlcDb) =>
        for {
          offerDbOpt <- dlcOfferDAO.findByParamHash(paramHash)
          acceptDbOpt <- dlcAcceptDAO.findByParamHash(paramHash)
          fundingInputDbs <- dlcInputsDAO.findByParamHash(paramHash)
          txIds = fundingInputDbs.map(_.outPoint.txIdBE)
          remotePrevTxs <- remoteTxDAO.findByTxIdBEs(txIds)
          localPrevTxs <- transactionDAO.findByTxIdBEs(txIds)
          refundSigDbs <- dlcRefundSigDAO.findByParamHash(paramHash)
          sigDbs <- dlcSigsDAO.findByParamHash(paramHash)

          res <- offerDbOpt match {
            case None => FutureUtil.none
            case Some(offerDb) =>
              val prevTxs = (remotePrevTxs ++ localPrevTxs).map(_.transaction)
              val txs = prevTxs.groupBy(_.txIdBE)

              val isInit = dlcDb.isInitiator

              val fundingInputs = fundingInputDbs.map(input =>
                input.toFundingInput(txs(input.outPoint.txIdBE).head))

              val offerRefundSigOpt =
                refundSigDbs.find(_.isInitiator).map(_.refundSig)
              val acceptRefundSigOpt =
                refundSigDbs.find(!_.isInitiator).map(_.refundSig)

              val offer = offerDb.toDLCOffer(fundingInputs)
              val acceptOpt = acceptDbOpt.map(
                _.toDLCAccept(fundingInputs,
                              sigDbs
                                .filter(!_.isInitiator)
                                .map(dbSig => (dbSig.outcome, dbSig.signature)),
                              acceptRefundSigOpt.get))

              val initSigs = sigDbs.filter(_.isInitiator)

              def getDLCSign: DLCSign = {
                val cetSigs =
                  CETSignatures(
                    initSigs.map(dbSig => (dbSig.outcome, dbSig.signature)),
                    offerRefundSigOpt.get)

                val contractId = dlcDb.contractIdOpt.get
                val fundingSigs =
                  fundingInputDbs
                    .filter(_.isInitiator)
                    .map { input =>
                      input.witnessScriptOpt match {
                        case Some(witnessScript) =>
                          witnessScript match {
                            case EmptyScriptWitness =>
                              throw new RuntimeException(
                                "Script witness cannot be empty")
                            case witness: ScriptWitnessV0 =>
                              (input.outPoint, witness)
                          }
                        case None =>
                          throw new RuntimeException("Must be segwit")
                      }
                    }

                DLCSign(cetSigs, FundingSignatures(fundingSigs), contractId)
              }

              def getFundingTx: Future[Transaction] = {
                transactionDAO
                  .read(dlcDb.fundingTxIdOpt.get)
                  .flatMap {
                    case Some(txDb) => Future.successful(txDb.transaction)
                    case None =>
                      Future.failed(new RuntimeException(
                        "Wallet does not have the funding transaction"))
                  }
              }

              def getClosingTx: Future[Transaction] = {
                transactionDAO
                  .read(dlcDb.closingTxIdOpt.get)
                  .flatMap {
                    case Some(txDb) => Future.successful(txDb.transaction)
                    case None =>
                      Future.failed(new RuntimeException(
                        "Wallet does not have the closing transaction"))
                  }
              }

              val statusF = dlcDb.state match {
                case Offered =>
                  Future.successful(DLCStatus.Offered(paramHash, isInit, offer))
                case Accepted =>
                  Future.successful(
                    DLCStatus
                      .Accepted(paramHash, isInit, offer, acceptOpt.get))
                case Signed =>
                  Future.successful(
                    DLCStatus
                      .Signed(paramHash,
                              isInit,
                              offer,
                              acceptOpt.get,
                              getDLCSign))
                case Broadcasted =>
                  getFundingTx.map(tx =>
                    DLCStatus.Broadcasted(paramHash,
                                          isInit,
                                          offer,
                                          acceptOpt.get,
                                          getDLCSign,
                                          tx))
                case Confirmed =>
                  getFundingTx.map(tx =>
                    DLCStatus.Confirmed(paramHash,
                                        isInit,
                                        offer,
                                        acceptOpt.get,
                                        getDLCSign,
                                        tx))
                case Claimed =>
                  for {
                    fundingTx <- getFundingTx
                    closingTx <- getClosingTx
                  } yield DLCStatus.Claimed(paramHash,
                                            isInit,
                                            offer,
                                            acceptOpt.get,
                                            getDLCSign,
                                            fundingTx,
                                            dlcDb.oracleSigsOpt.get,
                                            closingTx)
                case RemoteClaimed =>
                  for {
                    fundingTx <- getFundingTx
                    closingTx <- getClosingTx
                  } yield DLCStatus.RemoteClaimed(paramHash,
                                                  isInit,
                                                  offer,
                                                  acceptOpt.get,
                                                  getDLCSign,
                                                  fundingTx,
                                                  closingTx)
                case Refunded =>
                  for {
                    fundingTx <- getFundingTx
                    closingTx <- getClosingTx
                  } yield DLCStatus.Refunded(paramHash,
                                             isInit,
                                             offer,
                                             acceptOpt.get,
                                             getDLCSign,
                                             fundingTx,
                                             closingTx)
              }

              statusF.map(Some(_))
          }
        } yield res
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
