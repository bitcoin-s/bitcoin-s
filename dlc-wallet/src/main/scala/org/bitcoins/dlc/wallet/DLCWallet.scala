package org.bitcoins.dlc.wallet

import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency._
import org.bitcoins.core.hd._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.execution._
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models.DLCStatus._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.dlc.sign._
import org.bitcoins.core.protocol.dlc.verify.DLCSignatureVerifier
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BlockTimeStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.wallet.internal._
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletLogger}
import scodec.bits.ByteVector

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

abstract class DLCWallet
    extends Wallet
    with AnyDLCHDWalletApi
    with DLCDataManagement
    with DLCTransactionProcessing {

  implicit val dlcConfig: DLCAppConfig

  private[bitcoins] val announcementDAO: OracleAnnouncementDataDAO =
    OracleAnnouncementDataDAO()
  private[bitcoins] val oracleNonceDAO: OracleNonceDAO = OracleNonceDAO()

  private[bitcoins] val dlcAnnouncementDAO: DLCAnnouncementDAO =
    DLCAnnouncementDAO()
  private[bitcoins] val dlcOfferDAO: DLCOfferDAO = DLCOfferDAO()
  private[bitcoins] val dlcAcceptDAO: DLCAcceptDAO = DLCAcceptDAO()
  private[bitcoins] val dlcDAO: DLCDAO = DLCDAO()
  private[bitcoins] val dlcInputsDAO: DLCFundingInputDAO = DLCFundingInputDAO()
  private[bitcoins] val dlcSigsDAO: DLCCETSignatureDAO = DLCCETSignatureDAO()
  private[bitcoins] val dlcRefundSigDAO: DLCRefundSigDAO = DLCRefundSigDAO()
  private[bitcoins] val remoteTxDAO: DLCRemoteTxDAO = DLCRemoteTxDAO()

  private def calcContractId(offer: DLCOffer, accept: DLCAccept): ByteVector = {
    val builder = DLCTxBuilder(offer, accept.withoutSigs)
    builder.calcContractId
  }

  private def updateDLCContractIds(
      offer: DLCOffer,
      accept: DLCAccept): Future[DLCDb] = {
    require(accept.tempContractId == offer.tempContractId,
            "Offer and Accept have differing tempContractIds!")
    val dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))
    for {
      dlcOpt <- dlcDAO.read(dlcId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with dlcId ${dlcId.hex}"))
      }
      contractId = calcContractId(offer, accept)

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

  /** Updates the aggregate signature in the DLCDb corresponding to the given contractId */
  private def updateAggregateSignature(
      contractId: ByteVector,
      aggregateSignature: SchnorrDigitalSignature): Future[DLCDb] = {
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
        s"Updating DLC (${contractId.toHex}) aggregate signature to ${aggregateSignature.hex}")
      updated <- dlcDAO.update(
        dlcDb.copy(aggregateSignatureOpt = Some(aggregateSignature)))
    } yield updated
  }

  private def updateDLCOracleSigs(
      sigs: Vector[OracleSignatures]): Future[Vector[OracleNonceDb]] = {
    val outcomeAndSigByNonce = sigs.flatMap {
      case enum: EnumOracleSignature =>
        Vector((enum.sig.rx, (enum.getOutcome.outcome, enum.sig)))
      case numeric: NumericOracleSignatures =>
        val nonces = numeric.sigs.map(_.rx)
        val outcomes = numeric.getOutcome.digits.map(_.toString)
        val outcomeAndSigs = outcomes.zip(numeric.sigs)
        nonces.zip(outcomeAndSigs)
    }.toMap

    require(outcomeAndSigByNonce.forall(t => t._1 == t._2._2.rx),
            "nonces out of order")

    for {
      nonceDbs <- oracleNonceDAO.findByNonces(
        outcomeAndSigByNonce.keys.toVector)
      _ = assert(nonceDbs.size == outcomeAndSigByNonce.keys.size,
                 "Didn't receive all nonce dbs")

      updated = nonceDbs.map { db =>
        val (outcome, sig) = outcomeAndSigByNonce(db.nonce)
        db.copy(outcomeOpt = Some(outcome), signatureOpt = Some(sig))
      }

      updates <- oracleNonceDAO.updateAll(updated)

      announcementIds = updates.map(_.announcementId).distinct
      announcementDbs <- dlcAnnouncementDAO.findByAnnouncementIds(
        announcementIds)
      updatedDbs = announcementDbs.map(_.copy(used = true))
      _ <- dlcAnnouncementDAO.updateAll(updatedDbs)
    } yield updates
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

  override def cancelDLC(dlcId: Sha256Digest): Future[Unit] = {
    for {
      dlcOpt <- findDLC(dlcId)
      isInit = dlcOpt match {
        case Some(db) =>
          require(db.state == DLCState.Offered || db.state == DLCState.Accepted,
                  "Cannot cancel a DLC after it has been signed")
          db.isInitiator
        case None =>
          throw new IllegalArgumentException(
            s"No DLC Found with param hash ${dlcId.hex}")
      }

      inputs <- dlcInputsDAO.findByDLCId(dlcId, isInit)
      dbs <- spendingInfoDAO.findByOutPoints(inputs.map(_.outPoint))
      _ <- unmarkUTXOsAsReserved(dbs)

      _ <- dlcSigsDAO.deleteByDLCId(dlcId)
      _ <- dlcRefundSigDAO.deleteByDLCId(dlcId)
      _ <- dlcInputsDAO.deleteByDLCId(dlcId)
      _ <- dlcAcceptDAO.deleteByDLCId(dlcId)
      _ <- dlcOfferDAO.deleteByDLCId(dlcId)
      _ <- dlcAnnouncementDAO.deleteByDLCId(dlcId)
      _ <- dlcDAO.deleteByDLCId(dlcId)
    } yield ()
  }

  /** Creates a DLCOffer, if one has already been created
    * with the given parameters then that one will be returned instead.
    *
    * This is the first step of the initiator
    */
  override def createDLCOffer(
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLocktime: UInt32): Future[DLCOffer] = {
    logger.info("Creating DLC Offer")
    val announcements =
      contractInfo.oracleInfo.singleOracleInfos.map(_.announcement)

    val announcementDataDbs =
      OracleAnnouncementDbHelper.fromAnnouncements(announcements)

    for {
      feeRate <- determineFeeRate(feeRateOpt).map { fee =>
        SatoshisPerVirtualByte(fee.currencyUnit)
      }

      announcementDataDbs <- announcementDAO.createAll(announcementDataDbs)
      announcementsWithId = announcements.map { tlv =>
        val idOpt = announcementDataDbs
          .find(_.announcementSignature == tlv.announcementSignature)
          .flatMap(_.id)
        (tlv, idOpt.get)
      }
      nonceDbs = OracleNonceDbHelper.fromAnnouncements(announcementsWithId)
      _ <- oracleNonceDAO.upsertAll(nonceDbs)

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

      serialIds = DLCMessage.genSerialIds(spendingInfos.size)
      utxos = spendingInfos.zip(serialIds).map { case (utxo, id) =>
        DLCFundingInput.fromInputSigningInfo(utxo, id)
      }

      dlcId = calcDLCId(utxos.map(_.outPoint))
      dlcAnnouncementDbs = announcementDataDbs.zipWithIndex.map {
        case (a, index) =>
          DLCAnnouncementDb(dlcId = dlcId,
                            announcementId = a.id.get,
                            index = index,
                            used = false)
      }

      changeSPK =
        txBuilder.finalizer.changeSPK
          .asInstanceOf[WitnessScriptPubKey]
      network = networkParameters.asInstanceOf[BitcoinNetwork]
      changeAddr = Bech32Address(changeSPK, network)

      dlcPubKeys = calcDLCPubKeys(account.xpub, nextIndex)

      _ = logger.debug(
        s"DLC Offer data collected, creating database entry, ${dlcId.hex}")

      payoutSerialId = DLCMessage.genSerialId()
      changeSerialId = DLCMessage.genSerialId()
      fundOutputSerialId = DLCMessage.genSerialId(Vector(changeSerialId))

      timeouts = DLCTimeouts(BlockTimeStamp(locktime),
                             BlockTimeStamp(refundLocktime))

      offer = DLCOffer(
        contractInfo = contractInfo,
        pubKeys = dlcPubKeys,
        totalCollateral = collateral.satoshis,
        fundingInputs = utxos,
        changeAddress = changeAddr,
        payoutSerialId = payoutSerialId,
        changeSerialId = changeSerialId,
        fundOutputSerialId = fundOutputSerialId,
        feeRate = feeRate,
        timeouts = timeouts
      )

      oracleParamsOpt = OracleInfo.getOracleParamsOpt(contractInfo.oracleInfo)

      dlcDb = DLCDb(
        dlcId = dlcId,
        tempContractId = offer.tempContractId,
        contractIdOpt = None,
        protocolVersion = 0,
        state = DLCState.Offered,
        isInitiator = true,
        account = account.hdAccount,
        keyIndex = nextIndex,
        changeIndex = HDChainType.External,
        oracleThreshold = contractInfo.oracleInfo.threshold,
        oracleParamsTLVOpt = oracleParamsOpt,
        contractDescriptorTLV = contractInfo.contractDescriptor.toTLV,
        contractMaturity = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout,
        totalCollateral = contractInfo.totalCollateral,
        feeRate = feeRate,
        fundOutputSerialId = fundOutputSerialId,
        fundingOutPointOpt = None,
        fundingTxIdOpt = None,
        closingTxIdOpt = None,
        aggregateSignatureOpt = None
      )

      _ <- dlcDAO.create(dlcDb)
      _ <- dlcAnnouncementDAO.createAll(dlcAnnouncementDbs)

      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(dlcId, offer)

      dlcInputs = spendingInfos.zip(utxos).map { case (utxo, fundingInput) =>
        DLCFundingInputDb(
          dlcId = dlcId,
          isInitiator = true,
          inputSerialId = fundingInput.inputSerialId,
          outPoint = utxo.outPoint,
          output = utxo.output,
          maxWitnessLength = fundingInput.maxWitnessLen.toLong,
          redeemScriptOpt = InputInfo.getRedeemScript(utxo.inputInfo),
          witnessScriptOpt = InputInfo.getScriptWitness(utxo.inputInfo)
        )
      }

      _ = logger.info(
        s"Created offer with tempContractId ${offer.tempContractId.hex}")
      _ <- dlcInputsDAO.createAll(dlcInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
    } yield offer
  }

  private def initDLCForAccept(offer: DLCOffer): Future[(DLCDb, AccountDb)] = {
    logger.info(
      s"Initializing DLC from received offer with tempContractId ${offer.tempContractId.hex}")
    dlcDAO.findByTempContractId(offer.tempContractId).flatMap {
      case Some(dlcDb) =>
        accountDAO
          .findByAccount(dlcDb.account)
          .map(account => (dlcDb, account.get))
      case None =>
        val announcements =
          offer.contractInfo.oracleInfo.singleOracleInfos.map(_.announcement)

        val announcementDataDbs =
          OracleAnnouncementDbHelper.fromAnnouncements(announcements)

        val contractInfo = offer.contractInfo

        val dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          nextIndex <- getNextAvailableIndex(account, HDChainType.External)
          dlc = {
            val oracleParamsOpt =
              OracleInfo.getOracleParamsOpt(contractInfo.oracleInfo)

            DLCDb(
              dlcId = dlcId,
              tempContractId = offer.tempContractId,
              contractIdOpt = None,
              protocolVersion = 0,
              state = DLCState.Accepted,
              isInitiator = false,
              account = account.hdAccount,
              keyIndex = nextIndex,
              changeIndex = HDChainType.External,
              oracleThreshold = contractInfo.oracleInfo.threshold,
              oracleParamsTLVOpt = oracleParamsOpt,
              contractDescriptorTLV = contractInfo.contractDescriptor.toTLV,
              contractMaturity = offer.timeouts.contractMaturity,
              contractTimeout = offer.timeouts.contractTimeout,
              totalCollateral = contractInfo.totalCollateral,
              feeRate = offer.feeRate,
              fundOutputSerialId = offer.fundOutputSerialId,
              fundingOutPointOpt = None,
              fundingTxIdOpt = None,
              closingTxIdOpt = None,
              aggregateSignatureOpt = None
            )
          }
          _ <- writeDLCKeysToAddressDb(account, nextIndex)
          writtenDLC <- dlcDAO.create(dlc)

          announcementDataDbs <- announcementDAO.createAll(announcementDataDbs)
          announcementsWithId = announcements.map { tlv =>
            val idOpt = announcementDataDbs
              .find(_.announcementSignature == tlv.announcementSignature)
              .flatMap(_.id)
            (tlv, idOpt.get)
          }
          nonceDbs = OracleNonceDbHelper.fromAnnouncements(announcementsWithId)
          _ <- oracleNonceDAO.upsertAll(nonceDbs)

          dlcAnnouncementDbs = announcementDataDbs.zipWithIndex.map {
            case (a, index) =>
              DLCAnnouncementDb(dlcId = dlcId,
                                announcementId = a.id.get,
                                index = index,
                                used = false)
          }
          _ <- dlcAnnouncementDAO.createAll(dlcAnnouncementDbs)
        } yield (writtenDLC, account)
    }
  }

  /** Creates a DLCAccept from the default Segwit account from a given offer, if one has already been
    * created with the given parameters then that one will be returned instead.
    *
    * This is the first step of the recipient
    */
  override def acceptDLCOffer(offer: DLCOffer): Future[DLCAccept] = {
    logger.debug("Calculating relevant wallet data for DLC Accept")

    val dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

    val collateral = offer.contractInfo.max - offer.totalCollateral

    logger.debug(s"Checking if Accept (${dlcId.hex}) has already been made")
    for {
      (dlc, account) <- initDLCForAccept(offer)
      dlcAcceptDbOpt <- dlcAcceptDAO.findByDLCId(dlcId)
      dlcAccept <- dlcAcceptDbOpt match {
        case Some(dlcAcceptDb) =>
          logger.debug(
            s"DLC Accept (${dlcId.hex}) has already been made, returning accept")
          for {
            fundingInputs <-
              dlcInputsDAO.findByDLCId(dlc.dlcId, isInitiator = false)
            prevTxs <-
              transactionDAO.findByTxIdBEs(fundingInputs.map(_.outPoint.txIdBE))
            outcomeSigDbs <- dlcSigsDAO.findByDLCId(dlcId)
            refundSigDb <- dlcRefundSigDAO.read(dlcId)
          } yield {
            val inputRefs = matchPrevTxsWithInputs(fundingInputs, prevTxs)

            dlcAcceptDb.toDLCAccept(offer.tempContractId,
                                    inputRefs,
                                    outcomeSigDbs.map { db =>
                                      db.sigPoint -> db.acceptSig
                                    },
                                    refundSigDb.get.acceptSig)
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

      serialIds = DLCMessage.genSerialIds(
        spendingInfos.size,
        offer.fundingInputs.map(_.inputSerialId))
      utxos = spendingInfos.zip(serialIds).map { case (utxo, id) =>
        DLCFundingInput.fromInputSigningInfo(utxo, id)
      }

      changeSPK = txBuilder.finalizer.changeSPK.asInstanceOf[P2WPKHWitnessSPKV0]
      changeAddr = Bech32Address(changeSPK, network)

      bip32Path = BIP32Path(
        account.hdAccount.path ++ Vector(BIP32Node(0, hardened = false),
                                         BIP32Node(dlc.keyIndex,
                                                   hardened = false)))

      privKeyPath = HDPath.fromString(bip32Path.toString)
      fundingPrivKey =
        keyManager.toSign(privKeyPath)

      dlcPubKeys = calcDLCPubKeys(account.xpub, dlc.keyIndex)

      _ = require(dlcPubKeys.fundingKey == fundingPrivKey.publicKey,
                  "Did not derive the same funding private and public key")

      payoutSerialId = DLCMessage.genSerialId(Vector(offer.payoutSerialId))
      changeSerialId = DLCMessage.genSerialId(
        Vector(offer.fundOutputSerialId, offer.changeSerialId))

      acceptWithoutSigs = DLCAcceptWithoutSigs(
        totalCollateral = collateral.satoshis,
        pubKeys = dlcPubKeys,
        fundingInputs = utxos,
        changeAddress = changeAddr,
        payoutSerialId = payoutSerialId,
        changeSerialId = changeSerialId,
        negotiationFields = DLCAccept.NoNegotiationFields,
        tempContractId = offer.tempContractId
      )

      builder = DLCTxBuilder(offer, acceptWithoutSigs)

      contractId = builder.calcContractId

      signer = DLCTxSigner(builder = builder,
                           isInitiator = false,
                           fundingKey = fundingPrivKey,
                           finalAddress = dlcPubKeys.payoutAddress,
                           fundingUtxos = spendingInfos)

      spkDb = ScriptPubKeyDb(builder.fundingSPK)
      // only update spk db if we don't have it
      spkDbOpt <- scriptPubKeyDAO.findScriptPubKey(spkDb.scriptPubKey)
      _ <- spkDbOpt match {
        case Some(_) => Future.unit
        case None    => scriptPubKeyDAO.create(spkDb)
      }

      _ = logger.info(s"Creating CET Sigs for ${contractId.toHex}")
      cetSigs <- signer.createCETSigsAsync()

      _ = logger.debug(
        s"DLC Accept data collected, creating database entry, ${dlc.dlcId.hex}")

      dlcAcceptDb = DLCAcceptDb(
        dlcId = dlc.dlcId,
        fundingKey = dlcPubKeys.fundingKey,
        payoutAddress = dlcPubKeys.payoutAddress,
        payoutSerialId = payoutSerialId,
        collateral = collateral,
        changeAddress = changeAddr,
        changeSerialId = changeSerialId
      )

      sigsDbs = cetSigs.outcomeSigs.zipWithIndex.map { case (sig, index) =>
        DLCCETSignatureDb(dlc.dlcId, index = index, sig._1, sig._2, None)
      }

      refundSigDb =
        DLCRefundSigDb(dlc.dlcId, cetSigs.refundSig, None)

      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(dlc.dlcId, offer)

      offerInputs = offer.fundingInputs.map(funding =>
        DLCFundingInputDb(
          dlcId = dlc.dlcId,
          isInitiator = true,
          inputSerialId = funding.inputSerialId,
          outPoint = funding.outPoint,
          output = funding.output,
          maxWitnessLength = funding.maxWitnessLen.toLong,
          redeemScriptOpt = funding.redeemScriptOpt,
          witnessScriptOpt = None
        ))

      offerPrevTxs = offer.fundingInputs.map(funding =>
        TransactionDbHelper.fromTransaction(funding.prevTx,
                                            blockHashOpt = None))

      acceptInputs = spendingInfos.zip(utxos).map { case (utxo, fundingInput) =>
        DLCFundingInputDb(
          dlcId = dlc.dlcId,
          isInitiator = false,
          inputSerialId = fundingInput.inputSerialId,
          outPoint = utxo.outPoint,
          output = utxo.output,
          maxWitnessLength = fundingInput.maxWitnessLen.toLong,
          redeemScriptOpt = InputInfo.getRedeemScript(utxo.inputInfo),
          witnessScriptOpt = InputInfo.getScriptWitness(utxo.inputInfo)
        )
      }

      accept =
        dlcAcceptDb.toDLCAccept(dlc.tempContractId,
                                utxos,
                                cetSigs.outcomeSigs,
                                cetSigs.refundSig)

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

      fundingTx = builder.buildFundingTx
      outPoint = TransactionOutPoint(fundingTx.txId,
                                     UInt32(builder.fundOutputIndex))
      _ <- updateFundingOutPoint(dlcDb.contractIdOpt.get, outPoint)
    } yield accept
  }

  def registerDLCAccept(
      accept: DLCAccept): Future[(DLCDb, Vector[DLCCETSignatureDb])] = {
    logger.debug(
      s"Checking if DLC Accept with tempContractId ${accept.tempContractId.hex} has already been registered")
    val dbsF = for {
      dlcDbOpt <- dlcDAO.findByTempContractId(accept.tempContractId)
      (dlcDb, acceptDbOpt) <- dlcDbOpt match {
        case Some(db) =>
          dlcAcceptDAO
            .findByDLCId(db.dlcId)
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

        logger.debug(s"DLC Offer (${dlc.dlcId.hex}) found, adding accept data")

        val dlcId = dlc.dlcId
        val dlcAcceptDb = DLCAcceptDbHelper.fromDLCAccept(dlcId, accept)
        val acceptInputs = accept.fundingInputs.map(funding =>
          DLCFundingInputDb(
            dlcId = dlcId,
            isInitiator = false,
            inputSerialId = funding.inputSerialId,
            outPoint = funding.outPoint,
            output = funding.output,
            maxWitnessLength = funding.maxWitnessLen.toLong,
            redeemScriptOpt = funding.redeemScriptOpt,
            witnessScriptOpt = None
          ))

        val acceptPrevTxs = accept.fundingInputs.map { funding =>
          TransactionDbHelper.fromTransaction(funding.prevTx,
                                              blockHashOpt = None)
        }

        val sigsDbs = accept.cetSigs.outcomeSigs.zipWithIndex.map {
          case (sig, index) =>
            DLCCETSignatureDb(dlc.dlcId, index = index, sig._1, sig._2, None)
        }

        val refundSigDb =
          DLCRefundSigDb(dlcId, accept.cetSigs.refundSig, None)

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
          offerDb <- dlcOfferDAO.findByDLCId(dlc.dlcId).map(_.get)
          offerInputs <-
            dlcInputsDAO.findByDLCId(dlc.dlcId, isInitiator = true)
          prevTxs <-
            transactionDAO.findByTxIdBEs(offerInputs.map(_.outPoint.txIdBE))

          announcements <- dlcAnnouncementDAO.findByDLCId(dlcId)
          announcementIds = announcements.map(_.announcementId)
          announcementData <- announcementDAO.findByIds(announcementIds)
          nonceDbs <- oracleNonceDAO.findByAnnouncementIds(announcementIds)

          contractInfo = getContractInfo(dlc,
                                         announcements,
                                         announcementData,
                                         nonceDbs)

          offer =
            offerDb.toDLCOffer(contractInfo,
                               matchPrevTxsWithInputs(offerInputs, prevTxs),
                               dlc)

          dlcDb <- updateDLCContractIds(offer, accept)

          builder = DLCTxBuilder(offer, accept.withoutSigs)
          fundingTx = builder.buildFundingTx
          outPoint = TransactionOutPoint(fundingTx.txId,
                                         UInt32(builder.fundOutputIndex))

          spkDb = ScriptPubKeyDb(builder.fundingSPK)
          // only update spk db if we don't have it
          spkDbOpt <- scriptPubKeyDAO.findScriptPubKey(spkDb.scriptPubKey)
          _ <- spkDbOpt match {
            case Some(_) => Future.unit
            case None    => scriptPubKeyDAO.create(spkDb)
          }

          updatedDLCDb <-
            updateFundingOutPoint(dlcDb.contractIdOpt.get, outPoint)
        } yield (updatedDLCDb, sigsDbs)
      case (dlc, Some(_)) =>
        logger.debug(
          s"DLC Accept (${dlc.contractIdOpt.get.toHex}) has already been registered")
        dlcSigsDAO.findByDLCId(dlc.dlcId).map((dlc, _))
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
      (_, _, _, contractInfo) <- getDLCOfferData(dlcDb.dlcId)

      accept =
        DLCAccept.fromTLV(acceptTLV, walletConfig.network, contractInfo)
      dlcSign <- signDLC(accept)
    } yield dlcSign
  }

  /** Creates signatures for the DLCs CETs and Funding Inputs
    *
    * This is the second step of the initiator
    */
  override def signDLC(accept: DLCAccept): Future[DLCSign] = {
    for {
      (dlc, cetSigDbs) <- registerDLCAccept(accept)
      // .get should be safe now
      contractId = dlc.contractIdOpt.get

      signer <- signerFromDb(dlc.dlcId)

      mySigs <- dlcSigsDAO.findByDLCId(dlc.dlcId)
      refundSigDb <- dlcRefundSigDAO.findByDLCId(dlc.dlcId).map(_.get)
      cetSigs <-
        if (mySigs.forall(_.initiatorSig.isEmpty)) {
          logger.info(s"Creating CET Sigs for contract ${contractId.toHex}")
          for {
            sigs <- signer.createCETSigsAsync()

            sigDbs: Vector[DLCCETSignatureDb] = sigs.outcomeSigs
              .zip(cetSigDbs.sortBy(_.index))
              .map { case (sig, db) =>
                db.copy(initiatorSig = Some(sig._2))
              }
            _ <- dlcSigsDAO.updateAll(sigDbs)
          } yield sigs

        } else {
          logger.debug(s"CET Sigs already created for ${contractId.toHex}")
          val outcomeSigs = mySigs.map { dbSig =>
            dbSig.sigPoint -> dbSig.initiatorSig.get
          }

          val signatures = refundSigDb.initiatorSig match {
            case Some(sig) =>
              CETSignatures(outcomeSigs, sig)
            case None =>
              CETSignatures(outcomeSigs, signer.signRefundTx)
          }
          Future.successful(signatures)
        }

      _ = logger.info(s"Creating funding sigs for ${contractId.toHex}")
      fundingSigs <- Future.fromTry(signer.signFundingTx())

      updatedRefundSigDb = refundSigDb.copy(initiatorSig =
        Some(cetSigs.refundSig))
      _ <- dlcRefundSigDAO.update(updatedRefundSigDb)

      _ <- updateDLCState(dlc.contractIdOpt.get, DLCState.Signed)
      _ = logger.info(s"DLC ${contractId.toHex} is signed")
    } yield DLCSign(cetSigs, fundingSigs, contractId)
  }

  def verifyCETSigs(accept: DLCAccept): Future[Boolean] = {
    verifierFromAccept(accept).flatMap(
      _.verifyCETSigs(accept.cetSigs.indexedOutcomeSigs))
  }

  def verifyCETSigs(sign: DLCSign): Future[Boolean] = {
    verifierFromDb(sign.contractId).flatMap(
      _.verifyCETSigs(sign.cetSigs.indexedOutcomeSigs))
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
      inputs <- dlcInputsDAO.findByDLCId(dlc.dlcId)

      _ = logger.info(
        s"Verifying ${sign.fundingSigs.length} funding sigs for contract ${sign.contractId.toHex}")
      isValid <- verifyFundingSigs(inputs = inputs, sign = sign)
      _ <- {
        if (!isValid)
          Future.failed(new IllegalArgumentException(
            s"Funding Signatures provided are not valid! got ${sign.fundingSigs}"))
        else FutureUtil.unit
      }

      updatedInputs = sign.fundingSigs.map { case (outPoint, witness) =>
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
      offerDbOpt <- dlcOfferDAO.findByDLCId(dlcDb.dlcId)
      // .get should be safe now
      offerDb = offerDbOpt.get
      fundingInputDbs <- dlcInputsDAO.findByDLCId(dlcDb.dlcId)

      txIds = fundingInputDbs.map(_.outPoint.txIdBE)
      remotePrevTxs <- remoteTxDAO.findByTxIdBEs(txIds)
      localPrevTxs <- transactionDAO.findByTxIdBEs(txIds)

      announcements <- dlcAnnouncementDAO.findByDLCId(dlcDb.dlcId)
      announcementIds = announcements.map(_.announcementId)
      announcementData <- announcementDAO.findByIds(announcementIds)
      nonceDbs <- oracleNonceDAO.findByAnnouncementIds(announcementIds)

      prevTxs = (remotePrevTxs ++ localPrevTxs).map(_.transaction)
      txs = prevTxs.groupBy(_.txIdBE)

      fundingInputs = fundingInputDbs.map(input =>
        input.toFundingInput(txs(input.outPoint.txIdBE).head))

      contractInfo = getContractInfo(dlcDb,
                                     announcements,
                                     announcementData,
                                     nonceDbs)

      offer = offerDb.toDLCOffer(contractInfo, fundingInputs, dlcDb)

      sign = DLCSign.fromTLV(signTLV, offer)
      result <- addDLCSigs(sign)
    } yield result
  }

  /** Inputs the received signatures for a DLC into our database
    *
    * This is the second step of the recipient
    */
  override def addDLCSigs(sign: DLCSign): Future[DLCDb] = {
    dlcDAO.findByContractId(sign.contractId).flatMap {
      case Some(dlc) =>
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

          refundSigDb <- dlcRefundSigDAO.findByDLCId(dlc.dlcId).map(_.get)
          sigsDbs <- dlcSigsDAO.findByDLCId(dlc.dlcId)

          updatedRefund = refundSigDb.copy(initiatorSig =
            Some(sign.cetSigs.refundSig))
          updatedSigDbs = sigsDbs
            .sortBy(_.index)
            .zip(sign.cetSigs.outcomeSigs)
            .map { case (db, (_, sig)) =>
              db.copy(initiatorSig = Some(sig))
            }

          _ = logger.info(
            s"CET Signatures are valid for contract ${sign.contractId.toHex}")

          _ <- addFundingSigs(sign)
          _ <- dlcSigsDAO.updateAll(updatedSigDbs)
          _ <- dlcRefundSigDAO.update(updatedRefund)
          updated <- dlcDAO.update(dlc.updateState(DLCState.Signed))
          _ = logger.info(
            s"DLC ${sign.contractId.toHex} sigs are verified and stored, ready to broadcast")
        } yield updated
      case None =>
        Future.failed(new NoSuchElementException(
          s"No DLC found with corresponding contractId ${sign.contractId.toHex}"))
    }
  }

  private def verifierFromAccept(
      accept: DLCAccept): Future[DLCSignatureVerifier] = {
    for {
      dlcDbOpt <- dlcDAO.findByTempContractId(accept.tempContractId)
      dlcDb = dlcDbOpt.get

      (_, dlcOffer, fundingInputsDb, contractInfo) <- getDLCOfferData(
        dlcDb.dlcId)

      localFundingInputs = fundingInputsDb.filter(_.isInitiator)

      prevTxs <-
        transactionDAO.findByTxIdBEs(fundingInputsDb.map(_.outPoint.txIdBE))
    } yield {
      val offerFundingInputs =
        matchPrevTxsWithInputs(localFundingInputs, prevTxs)

      val builder =
        DLCTxBuilder(
          dlcOffer.toDLCOffer(contractInfo, offerFundingInputs, dlcDb),
          accept.withoutSigs)

      DLCSignatureVerifier(builder, dlcDb.isInitiator)
    }
  }

  private def verifierFromDb(
      contractId: ByteVector): Future[DLCSignatureVerifier] = {
    getDLCFundingData(contractId).flatMap {
      case (dlcDb, dlcOffer, dlcAccept, fundingInputsDb, contractInfo) =>
        verifierFromDbData(dlcDb,
                           dlcOffer,
                           dlcAccept,
                           fundingInputsDb,
                           contractInfo)
    }
  }

  private def builderFromDbData(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo): Future[DLCTxBuilder] = {
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

      val offer = dlcOffer.toDLCOffer(contractInfo,
                                      offerFundingInputs,
                                      dlcDb.fundOutputSerialId,
                                      dlcDb.feeRate,
                                      dlcDb.dlcTimeouts)

      val accept = dlcAccept.toDLCAcceptWithoutSigs(dlcDb.tempContractId,
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
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo): Future[DLCSignatureVerifier] = {
    val builderF =
      builderFromDbData(dlcDb,
                        dlcOffer,
                        dlcAccept,
                        fundingInputsDb,
                        contractInfo)

    builderF.map(DLCSignatureVerifier(_, dlcDb.isInitiator))
  }

  private def signerFromDb(dlcId: Sha256Digest): Future[DLCTxSigner] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputsDb, contractInfo) <-
        getDLCFundingData(dlcId)
      signer <- signerFromDb(dlcDb,
                             dlcOffer,
                             dlcAccept,
                             fundingInputsDb,
                             contractInfo)
    } yield signer
  }

  private def signerFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo): Future[DLCTxSigner] = {
    for {
      fundingUtxos <- fundingUtxosFromDb(dlcDb, fundingInputsDb)
      builder <- builderFromDbData(dlcDb = dlcDb,
                                   dlcOffer = dlcOffer,
                                   dlcAccept = dlcAccept,
                                   fundingInputsDb = fundingInputsDb,
                                   contractInfo = contractInfo)
    } yield {
      val (fundingKey, payoutAddress) = if (dlcDb.isInitiator) {
        (dlcOffer.fundingKey, dlcOffer.payoutAddress)
      } else {
        (dlcAccept.fundingKey, dlcAccept.payoutAddress)
      }

      val bip32Path = BIP32Path(
        dlcDb.account.path ++ Vector(
          BIP32Node(dlcDb.changeIndex.index, hardened = false),
          BIP32Node(dlcDb.keyIndex, hardened = false)))

      val privKeyPath = HDPath.fromString(bip32Path.toString)
      val fundingPrivKey = keyManager.toSign(privKeyPath)

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
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo): Future[DLCExecutor] = {
    signerFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputsDb, contractInfo).map(
      DLCExecutor.apply)
  }

  private def executorFromDb(dlcId: Sha256Digest): Future[DLCExecutor] = {
    signerFromDb(dlcId).map(DLCExecutor.apply)
  }

  private[wallet] def executorAndSetupFromDb(
      contractId: ByteVector): Future[(DLCExecutor, SetupDLC)] = {
    getAllDLCData(contractId).flatMap {
      case (dlcDb,
            dlcOffer,
            dlcAccept,
            refundSigs,
            contractInfo,
            fundingInputsDb,
            outcomeSigDbs) =>
        executorAndSetupFromDb(dlcDb,
                               dlcOffer,
                               dlcAccept,
                               refundSigs,
                               contractInfo,
                               fundingInputsDb,
                               outcomeSigDbs)
    }
  }

  private[wallet] def executorAndSetupFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      refundSigDb: DLCRefundSigDb,
      contractInfo: ContractInfo,
      fundingInputs: Vector[DLCFundingInputDb],
      outcomeSigDbs: Vector[DLCCETSignatureDb]): Future[
    (DLCExecutor, SetupDLC)] = {

    executorFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputs, contractInfo)
      .flatMap { executor =>
        // Filter for only counter party's outcome sigs
        val outcomeSigs =
          if (dlcDb.isInitiator) {
            outcomeSigDbs
              .map { dbSig =>
                dbSig.sigPoint -> dbSig.acceptSig
              }
          } else {
            outcomeSigDbs
              .map { dbSig =>
                dbSig.sigPoint -> dbSig.initiatorSig.get
              }
          }

        val refundSig = if (dlcDb.isInitiator) {
          refundSigDb.acceptSig
        } else refundSigDb.initiatorSig.get

        val cetSigs = CETSignatures(outcomeSigs, refundSig)

        val setupF = if (dlcDb.isInitiator) {
          // Note that the funding tx in this setup is not signed
          executor.setupDLCOffer(cetSigs)
        } else {
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
          executor.setupDLCAccept(cetSigs, FundingSignatures(fundingSigs), None)
        }

        Future.fromTry(setupF.map((executor, _)))
      }
  }

  override def getDLCFundingTx(contractId: ByteVector): Future[Transaction] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, contractInfo) <-
        getDLCFundingData(contractId)

      signer <- signerFromDb(dlcDb,
                             dlcOffer,
                             dlcAccept,
                             fundingInputs,
                             contractInfo)
      fundingTx <-
        if (dlcDb.isInitiator) {
          transactionDAO.findByTxId(signer.builder.buildFundingTx.txIdBE).map {
            case Some(txDb) => txDb.transaction
            case None =>
              signer.builder.buildFundingTx
          }
        } else {
          Future.fromTry {
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
            signer.completeFundingTx(FundingSignatures(remoteSigs))
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
      sigs: Seq[OracleAttestmentTLV]): Future[Transaction] = {
    require(sigs.nonEmpty, "Must provide at least one oracle signature")

    for {
      dlcDb <- dlcDAO.findByContractId(contractId).map(_.get)
      announcements <- dlcAnnouncementDAO.findByDLCId(dlcDb.dlcId)
      announcementIds = announcements.map(_.announcementId)
      announcementData <- announcementDAO.findByIds(announcementIds)
      nonceDbs <- oracleNonceDAO.findByAnnouncementIds(announcementIds)
      announcementTLVs = getOracleAnnouncements(announcements,
                                                announcementData,
                                                nonceDbs)

      oracleSigs =
        sigs.foldLeft(Vector.empty[OracleSignatures]) { (acc, sig) =>
          // Nonces should be unique so searching for the first nonce should be safe
          val firstNonce = sig.sigs.head.rx
          announcementTLVs
            .find(
              _.eventTLV.nonces.headOption
                .contains(firstNonce)) match {
            case Some(announcement) =>
              acc :+ OracleSignatures(SingleOracleInfo(announcement), sig.sigs)
            case None =>
              throw new RuntimeException(
                s"Cannot find announcement for associated public key, ${sig.publicKey.hex}")
          }
        }

      tx <- executeDLC(contractId, oracleSigs)
    } yield tx
  }

  override def executeDLC(
      contractId: ByteVector,
      oracleSigs: Vector[OracleSignatures]): Future[Transaction] = {
    require(oracleSigs.nonEmpty, "Must provide at least one oracle signature")
    for {
      (executor, setup) <- executorAndSetupFromDb(contractId)

      executed = executor.executeDLC(setup, oracleSigs)
      (tx, outcome, sigsUsed) =
        (executed.cet, executed.outcome, executed.sigsUsed)
      _ = logger.info(
        s"Created DLC execution transaction ${tx.txIdBE.hex} for contract ${contractId.toHex}")

      _ <- updateDLCOracleSigs(sigsUsed)
      _ <- updateDLCState(contractId, DLCState.Claimed)
      _ <- updateClosingTxId(contractId, tx.txIdBE)

      oracleSigSum =
        OracleSignatures.computeAggregateSignature(outcome, sigsUsed)
      aggSig = SchnorrDigitalSignature(outcome.aggregateNonce,
                                       oracleSigSum.fieldElement)
      _ <- updateAggregateSignature(contractId, aggSig)

      _ <- processTransaction(tx, None)
    } yield tx
  }

  override def executeDLCRefund(contractId: ByteVector): Future[Transaction] = {
    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb = dlcDbOpt.get

      executor <- executorFromDb(dlcDb.dlcId)
      refundSigDbOpt <- dlcRefundSigDAO.findByDLCId(dlcDb.dlcId)

      refundSig =
        if (dlcDb.isInitiator) refundSigDbOpt.get.acceptSig
        else refundSigDbOpt.get.initiatorSig.get

      refundTx = executor.executeRefundDLC(refundSig).refundTx
      _ = logger.info(
        s"Created DLC refund transaction ${refundTx.txIdBE.hex} for contract ${contractId.toHex}")

      _ <- updateDLCState(contractId, DLCState.Refunded)
      _ <- updateClosingTxId(contractId, refundTx.txIdBE)

      _ <- processTransaction(refundTx, blockHashOpt = None)
    } yield refundTx
  }

  override def listDLCs(): Future[Vector[DLCStatus]] = {
    for {
      ids <- dlcDAO.findAll().map(_.map(_.dlcId))
      dlcFs = ids.map(findDLC)
      dlcs <- Future.sequence(dlcFs)
    } yield {
      dlcs.collect { case Some(dlc) =>
        dlc
      }
    }
  }

  override def findDLC(dlcId: Sha256Digest): Future[Option[DLCStatus]] = {
    for {
      dlcDbOpt <- dlcDAO.read(dlcId)
      offerDbOpt <- dlcOfferDAO.read(dlcId)
      announcements <- dlcAnnouncementDAO.findByDLCId(dlcId)
      announcementIds = announcements.map(_.announcementId)
      announcementData <- announcementDAO.findByIds(announcementIds)
      nonceDbs <- oracleNonceDAO.findByAnnouncementIds(announcementIds)
    } yield (dlcDbOpt, offerDbOpt) match {
      case (Some(dlcDb), Some(offerDb)) =>
        val totalCollateral = dlcDb.totalCollateral

        val localCollateral = if (dlcDb.isInitiator) {
          offerDb.collateral
        } else {
          totalCollateral - offerDb.collateral
        }

        val contractInfo =
          getContractInfo(dlcDb, announcements, announcementData, nonceDbs)

        // Only called when safe
        lazy val (oracleOutcome, sigs) = {
          val aggSig = dlcDb.aggregateSignatureOpt.get
          val outcome =
            contractInfo.sigPointMap(aggSig.sig.toPrivateKey.publicKey)

          val sigs = nonceDbs.flatMap(_.signatureOpt)

          (outcome, sigs)
        }

        val status = dlcDb.state match {
          case DLCState.Offered =>
            Offered(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              contractInfo,
              dlcDb.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral
            )
          case DLCState.Accepted =>
            Accepted(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              dlcDb.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral
            )
          case DLCState.Signed =>
            Signed(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              dlcDb.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral
            )
          case DLCState.Broadcasted =>
            Broadcasted(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              dlcDb.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get
            )
          case DLCState.Confirmed =>
            Confirmed(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              dlcDb.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get
            )
          case DLCState.Claimed =>
            Claimed(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              dlcDb.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get,
              dlcDb.closingTxIdOpt.get,
              sigs,
              oracleOutcome
            )
          case DLCState.RemoteClaimed =>
            RemoteClaimed(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              dlcDb.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get,
              dlcDb.closingTxIdOpt.get,
              dlcDb.aggregateSignatureOpt.get,
              oracleOutcome
            )
          case DLCState.Refunded =>
            Refunded(
              dlcId,
              dlcDb.isInitiator,
              dlcDb.tempContractId,
              dlcDb.contractIdOpt.get,
              contractInfo,
              dlcDb.dlcTimeouts,
              dlcDb.feeRate,
              totalCollateral,
              localCollateral,
              dlcDb.fundingTxIdOpt.get,
              dlcDb.closingTxIdOpt.get
            )
        }

        Some(status)
      case (None, None) | (None, Some(_)) | (Some(_), None) =>
        None
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
