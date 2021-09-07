package org.bitcoins.dlc.wallet

import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency._
import org.bitcoins.core.dlc.accounting.DLCWalletAccounting
import org.bitcoins.core.hd._
import org.bitcoins.core.number._
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCAccept._
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models.DLCState._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.dlc.sign._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.{FutureUtil, TimeUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.wallet.internal._
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.dlc.wallet.util.DLCStatusBuilder
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletLogger}
import scodec.bits.ByteVector

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

/** A [[Wallet]] with full DLC Functionality */
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

  private[bitcoins] val contractDataDAO: DLCContractDataDAO =
    DLCContractDataDAO()
  private[bitcoins] val dlcInputsDAO: DLCFundingInputDAO = DLCFundingInputDAO()
  private[bitcoins] val dlcSigsDAO: DLCCETSignaturesDAO = DLCCETSignaturesDAO()
  private[bitcoins] val dlcRefundSigDAO: DLCRefundSigsDAO = DLCRefundSigsDAO()
  private[bitcoins] val remoteTxDAO: DLCRemoteTxDAO = DLCRemoteTxDAO()

  private def calcContractId(offer: DLCOffer, accept: DLCAccept): ByteVector = {
    val builder = DLCTxBuilder(offer, accept.withoutSigs)
    builder.calcContractId
  }

  /** Updates the contract Id in the wallet database for the given offer and accept */
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

      newDLCDb = dlcDb.updateContractId(contractId)
      _ = logger.debug(s"Updating DLC contract Ids")
      updated <- dlcDAO.update(newDLCDb)
    } yield updated
  }

  /** Updates the [[DLCState]] of a DLC with the given contractId in the wallet's database */
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
      _ = logger.info(
        s"Updating DLC (${contractId.toHex}) closing txId to txIdBE=${txId.hex}")
      updated <- dlcDAO.update(dlcDb.updateClosingTxId(txId))
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
        dlcDb.updateAggregateSignature(aggregateSignature))
    } yield updated
  }

  /** Updates the signatures in the oracle nonce database */
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

  /** Calculates a [[DLCPublicKeys]] from the wallet's [[BIP39KeyManager]] */
  private def calcDLCPubKeys(
      xpub: ExtPublicKey,
      chainType: HDChainType,
      keyIndex: Int): DLCPublicKeys = {
    val chainIndex = chainType.index
    val fundingKey =
      xpub
        .deriveChildPubKey(BIP32Path.fromString(s"m/$chainIndex/$keyIndex"))
        .get
        .key

    val payoutKey =
      xpub
        .deriveChildPubKey(
          BIP32Path.fromString(s"m/$chainIndex/${keyIndex + 1}"))
        .get
        .key

    networkParameters match {
      case bitcoinNetwork: BitcoinNetwork =>
        DLCPublicKeys.fromPubKeys(fundingKey, payoutKey, bitcoinNetwork)
    }
  }

  /** Writes the addresses corresponding to wallet's keys in a DLC to the
    * address database, this includes the address associated with the funding public key
    */
  private def writeDLCKeysToAddressDb(
      account: AccountDb,
      chainType: HDChainType,
      index: Int): Future[Vector[AddressDb]] = {
    for {
      zero <- getAddress(account, chainType, index)
      one <- getAddress(account, chainType, index + 1)
    } yield {
      logger.debug(s"Wrote DLC key addresses to database using index $index")
      Vector(zero, one)
    }
  }

  /** If the DLC has not reached the Signed state, it can be canceled.
    * Canceling a DLC deletes all data about it from the database,
    * as well as unreserves the utxos associated with it.
    */
  override def cancelDLC(dlcId: Sha256Digest): Future[Unit] = {
    for {
      dlcOpt <- dlcDAO.read(dlcId)
      dlcDb = dlcOpt match {
        case Some(db) =>
          require(
            db.state == DLCState.Offered || db.state == DLCState.Accepted || db.state == DLCState.Signed,
            "Cannot cancel a DLC after it has been signed")
          db
        case None =>
          throw new IllegalArgumentException(
            s"No DLC Found with dlc id ${dlcId.hex}")
      }

      inputs <- dlcInputsDAO.findByDLCId(dlcId, dlcDb.isInitiator)
      dbs <- spendingInfoDAO.findByOutPoints(inputs.map(_.outPoint))
      // allow this to fail in the case they have already been unreserved
      _ <- unmarkUTXOsAsReserved(dbs).recover { case _: Throwable => () }

      _ <- dlcSigsDAO.deleteByDLCId(dlcId)
      _ <- dlcRefundSigDAO.deleteByDLCId(dlcId)
      _ <- dlcInputsDAO.deleteByDLCId(dlcId)
      _ <- dlcAcceptDAO.deleteByDLCId(dlcId)
      _ <- dlcOfferDAO.deleteByDLCId(dlcId)
      _ <- contractDataDAO.deleteByDLCId(dlcId)
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

    //hack for now to get around https://github.com/bitcoin-s/bitcoin-s/issues/3127
    //filter announcements that we already have in the db
    val groupedAnnouncementsF: Future[AnnouncementGrouping] = {
      groupByExistingAnnouncements(announcements)
    }

    val feeRateF = determineFeeRate(feeRateOpt).map { fee =>
      SatoshisPerVirtualByte(fee.currencyUnit)
    }

    for {
      feeRate <- feeRateF
      groupedAnnouncements <- groupedAnnouncementsF
      announcementDataDbs <- announcementDAO.createAll(
        groupedAnnouncements.newAnnouncements)
      allAnnouncementDbs =
        announcementDataDbs ++ groupedAnnouncements.existingAnnouncements

      newAnnouncements = announcements.filter(a =>
        groupedAnnouncements.newAnnouncements.exists(
          _.announcementSignature == a.announcementSignature))

      newAnnouncementsWithId = newAnnouncements.map { tlv =>
        val idOpt: Option[Long] =
          announcementDataDbs
            .find(_.announcementSignature == tlv.announcementSignature)
            .flatMap(_.id)
        (tlv, idOpt.get)
      }
      nonceDbs = OracleNonceDbHelper.fromAnnouncements(newAnnouncementsWithId)
      _ <- oracleNonceDAO.createAll(nonceDbs)
      chainType = HDChainType.External

      account <- getDefaultAccountForType(AddressType.SegWit)
      nextIndex <- getNextAvailableIndex(account, chainType)
      _ <- writeDLCKeysToAddressDb(account, chainType, nextIndex)

      (txBuilder, spendingInfos) <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = feeRate,
        fromAccount = account,
        fromTagOpt = None,
        markAsReserved = true
      )

      serialIds = DLCMessage.genSerialIds(spendingInfos.size)
      utxos = spendingInfos.zip(serialIds).map { case (utxo, id) =>
        DLCFundingInput.fromInputSigningInfo(
          utxo,
          id,
          TransactionConstants.enableRBFSequence)
      }

      dlcId = calcDLCId(utxos.map(_.outPoint))
      dlcAnnouncementDbs = allAnnouncementDbs.zipWithIndex.map {
        case (a, index) =>
          DLCAnnouncementDb(dlcId = dlcId,
                            announcementId = a.id.get,
                            index = index,
                            used = false)
      }

      changeSPK =
        txBuilder.finalizer.changeSPK
      changeAddr = BitcoinAddress.fromScriptPubKey(changeSPK, networkParameters)

      dlcPubKeys = calcDLCPubKeys(account.xpub, chainType, nextIndex)

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
        changeIndex = chainType,
        keyIndex = nextIndex,
        feeRate = feeRate,
        fundOutputSerialId = fundOutputSerialId,
        lastUpdated = TimeUtil.now,
        fundingOutPointOpt = None,
        fundingTxIdOpt = None,
        closingTxIdOpt = None,
        aggregateSignatureOpt = None
      )

      contractDataDb = DLCContractDataDb(
        dlcId = dlcId,
        oracleThreshold = contractInfo.oracleInfo.threshold,
        oracleParamsTLVOpt = oracleParamsOpt,
        contractDescriptorTLV = contractInfo.contractDescriptor.toTLV,
        contractMaturity = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout,
        totalCollateral = contractInfo.totalCollateral
      )

      _ <- dlcDAO.create(dlcDb)
      _ <- contractDataDAO.create(contractDataDb)
      _ <- dlcAnnouncementDAO.createAll(dlcAnnouncementDbs)
      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(dlcId, offer)

      dlcInputs = spendingInfos.zip(utxos).zipWithIndex.map {
        case ((utxo, fundingInput), idx) =>
          DLCFundingInputDb(
            dlcId = dlcId,
            isInitiator = true,
            index = idx,
            inputSerialId = fundingInput.inputSerialId,
            outPoint = utxo.outPoint,
            output = utxo.output,
            nSequence = fundingInput.sequence,
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

        //filter announcements that we already have in the db
        val groupedAnnouncementsF: Future[AnnouncementGrouping] = {
          groupByExistingAnnouncements(announcements)
        }

        val contractInfo = offer.contractInfo

        val dlcId = calcDLCId(offer.fundingInputs.map(_.outPoint))

        val chainType = HDChainType.External

        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          nextIndex <- getNextAvailableIndex(account, chainType)
          dlc =
            DLCDb(
              dlcId = dlcId,
              tempContractId = offer.tempContractId,
              contractIdOpt = None,
              protocolVersion = 0,
              state = DLCState.Accepted,
              isInitiator = false,
              account = account.hdAccount,
              changeIndex = chainType,
              keyIndex = nextIndex,
              feeRate = offer.feeRate,
              fundOutputSerialId = offer.fundOutputSerialId,
              lastUpdated = TimeUtil.now,
              fundingOutPointOpt = None,
              fundingTxIdOpt = None,
              closingTxIdOpt = None,
              aggregateSignatureOpt = None
            )

          contractDataDb = {
            val oracleParamsOpt =
              OracleInfo.getOracleParamsOpt(contractInfo.oracleInfo)
            DLCContractDataDb(
              dlcId = dlcId,
              oracleThreshold = contractInfo.oracleInfo.threshold,
              oracleParamsTLVOpt = oracleParamsOpt,
              contractDescriptorTLV = contractInfo.contractDescriptor.toTLV,
              contractMaturity = offer.timeouts.contractMaturity,
              contractTimeout = offer.timeouts.contractTimeout,
              totalCollateral = contractInfo.totalCollateral
            )
          }

          _ <- writeDLCKeysToAddressDb(account, chainType, nextIndex)
          writtenDLC <- dlcDAO.create(dlc)
          _ <- contractDataDAO.create(contractDataDb)

          groupedAnnouncements <- groupedAnnouncementsF
          createdDbs <- announcementDAO.createAll(
            groupedAnnouncements.newAnnouncements)

          announcementDataDbs =
            createdDbs ++ groupedAnnouncements.existingAnnouncements

          newAnnouncements = announcements.filter(a =>
            groupedAnnouncements.newAnnouncements.exists(
              _.announcementSignature == a.announcementSignature))

          newAnnouncementsWithId = newAnnouncements.map { tlv =>
            val idOpt = createdDbs
              .find(_.announcementSignature == tlv.announcementSignature)
              .flatMap(_.id)
            (tlv, idOpt.get)
          }
          nonceDbs = OracleNonceDbHelper.fromAnnouncements(
            newAnnouncementsWithId)
          _ <- oracleNonceDAO.createAll(nonceDbs)

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
            outcomeSigsDbs <- dlcSigsDAO.findByDLCId(dlcId)
            refundSigsDb <- dlcRefundSigDAO.read(dlcId)
          } yield {
            val inputRefs = matchPrevTxsWithInputs(fundingInputs, prevTxs)

            dlcAcceptDb.toDLCAccept(offer.tempContractId,
                                    inputRefs,
                                    outcomeSigsDbs.map { db =>
                                      db.sigPoint -> db.accepterSig
                                    },
                                    refundSigsDb.get.accepterSig)
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

      serialIds = DLCMessage.genSerialIds(
        spendingInfos.size,
        offer.fundingInputs.map(_.inputSerialId))
      utxos = spendingInfos.zip(serialIds).map { case (utxo, id) =>
        DLCFundingInput.fromInputSigningInfo(
          utxo,
          id,
          TransactionConstants.enableRBFSequence)
      }

      changeSPK = txBuilder.finalizer.changeSPK
      changeAddr = BitcoinAddress.fromScriptPubKey(changeSPK, networkParameters)

      bip32Path = BIP32Path(
        account.hdAccount.path ++ Vector(BIP32Node(0, hardened = false),
                                         BIP32Node(dlc.keyIndex,
                                                   hardened = false)))

      privKeyPath = HDPath.fromString(bip32Path.toString)
      fundingPrivKey =
        keyManager.toSign(privKeyPath)

      dlcPubKeys = calcDLCPubKeys(account.xpub, dlc.changeIndex, dlc.keyIndex)

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
        changeSerialId = changeSerialId,
        negotiationFieldsTLV = NoNegotiationFields.toTLV
      )

      sigsDbs = cetSigs.outcomeSigs.zipWithIndex.map { case (sig, index) =>
        DLCCETSignaturesDb(dlc.dlcId, index = index, sig._1, sig._2, None)
      }

      refundSigsDb =
        DLCRefundSigsDb(dlc.dlcId, cetSigs.refundSig, None)

      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(dlc.dlcId, offer)

      offerInputs = offer.fundingInputs.zipWithIndex.map {
        case (funding, idx) =>
          DLCFundingInputDb(
            dlcId = dlc.dlcId,
            isInitiator = true,
            index = idx,
            inputSerialId = funding.inputSerialId,
            outPoint = funding.outPoint,
            output = funding.output,
            nSequence = funding.sequence,
            maxWitnessLength = funding.maxWitnessLen.toLong,
            redeemScriptOpt = funding.redeemScriptOpt,
            witnessScriptOpt = None
          )
      }

      offerPrevTxs = offer.fundingInputs.map(funding =>
        TransactionDbHelper.fromTransaction(funding.prevTx,
                                            blockHashOpt = None))

      acceptInputs = spendingInfos.zip(utxos).zipWithIndex.map {
        case ((utxo, fundingInput), idx) =>
          DLCFundingInputDb(
            dlcId = dlc.dlcId,
            isInitiator = false,
            index = idx,
            inputSerialId = fundingInput.inputSerialId,
            outPoint = utxo.outPoint,
            output = utxo.output,
            nSequence = fundingInput.sequence,
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
      _ <- dlcRefundSigDAO.create(refundSigsDb)
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
      accept: DLCAccept): Future[(DLCDb, Vector[DLCCETSignaturesDb])] = {
    logger.debug(
      s"Checking if DLC Accept with tempContractId ${accept.tempContractId.hex} has already been registered")
    val dbsF = for {
      dlcDbOpt <- dlcDAO.findByTempContractId(accept.tempContractId)
      (dlcDb, acceptDbOpt) <- dlcDbOpt match {
        case Some(db) =>
          require(db.isInitiator,
                  "Cannot call DLC Sign on our own DLC Accept message")
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
        lazy val dlcAcceptDb = DLCAcceptDbHelper.fromDLCAccept(dlcId, accept)
        lazy val acceptInputs = accept.fundingInputs.zipWithIndex.map {
          case (funding, idx) =>
            DLCFundingInputDb(
              dlcId = dlcId,
              isInitiator = false,
              index = idx,
              inputSerialId = funding.inputSerialId,
              outPoint = funding.outPoint,
              output = funding.output,
              nSequence = funding.sequence,
              maxWitnessLength = funding.maxWitnessLen.toLong,
              redeemScriptOpt = funding.redeemScriptOpt,
              witnessScriptOpt = None
            )
        }

        lazy val acceptPrevTxs = accept.fundingInputs.map { funding =>
          TransactionDbHelper.fromTransaction(funding.prevTx,
                                              blockHashOpt = None)
        }

        lazy val sigsDbs = accept.cetSigs.outcomeSigs.zipWithIndex.map {
          case (sig, index) =>
            DLCCETSignaturesDb(dlc.dlcId, index = index, sig._1, sig._2, None)
        }

        lazy val refundSigsDb =
          DLCRefundSigsDb(dlcId, accept.cetSigs.refundSig, None)

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

          _ <- remoteTxDAO.upsertAll(acceptPrevTxs)
          _ <- dlcInputsDAO.createAll(acceptInputs)
          _ <- dlcSigsDAO.createAll(sigsDbs)
          _ <- dlcRefundSigDAO.upsert(refundSigsDb)
          _ <- dlcAcceptDAO.upsert(dlcAcceptDb)

          // .get is safe here because we must have an offer if we have a dlcDAO
          offerDb <- dlcOfferDAO.findByDLCId(dlc.dlcId).map(_.get)
          offerInputs <-
            dlcInputsDAO.findByDLCId(dlc.dlcId, isInitiator = true)
          prevTxs <-
            transactionDAO.findByTxIdBEs(offerInputs.map(_.outPoint.txIdBE))

          contractData <- contractDataDAO.read(dlcId).map(_.get)
          (announcements, announcementData, nonceDbs) <- getDLCAnnouncementDbs(
            dlcId)

          contractInfo = getContractInfo(contractData,
                                         announcements,
                                         announcementData,
                                         nonceDbs)

          offer =
            offerDb.toDLCOffer(contractInfo,
                               matchPrevTxsWithInputs(offerInputs, prevTxs),
                               dlc,
                               contractData)

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

          updatedDLCDb <- dlcDAO.update(
            dlcDb
              .updateState(DLCState.Accepted)
              .updateFundingOutPoint(outPoint))
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
      contractInfo <- getContractInfo(dlcDb.dlcId)

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
      (dlc, cetSigsDbs) <- registerDLCAccept(accept)
      // .get should be safe now
      contractId = dlc.contractIdOpt.get

      signer <- signerFromDb(dlc.dlcId)

      mySigs <- dlcSigsDAO.findByDLCId(dlc.dlcId)
      refundSigsDb <- dlcRefundSigDAO.findByDLCId(dlc.dlcId).map(_.get)
      cetSigs <-
        if (mySigs.forall(_.initiatorSig.isEmpty)) {
          logger.info(s"Creating CET Sigs for contract ${contractId.toHex}")
          for {
            sigs <- signer.createCETSigsAsync()

            sigsDbs: Vector[DLCCETSignaturesDb] = sigs.outcomeSigs
              .zip(cetSigsDbs.sortBy(_.index))
              .map { case (sig, db) =>
                db.copy(initiatorSig = Some(sig._2))
              }
            _ <- dlcSigsDAO.updateAll(sigsDbs)
          } yield sigs

        } else {
          logger.debug(s"CET Sigs already created for ${contractId.toHex}")
          val outcomeSigs = mySigs.map { dbSig =>
            dbSig.sigPoint -> dbSig.initiatorSig.get
          }

          val signatures = refundSigsDb.initiatorSig match {
            case Some(sig) =>
              CETSignatures(outcomeSigs, sig)
            case None =>
              CETSignatures(outcomeSigs, signer.signRefundTx)
          }
          Future.successful(signatures)
        }

      _ = logger.info(s"Creating funding sigs for ${contractId.toHex}")
      fundingSigs <- Future.fromTry(signer.signFundingTx())

      // order fundingSigs by index
      inputDbs <- dlcInputsDAO.findByDLCId(dlc.dlcId, isInitiator = true)
      sortedSigVec = inputDbs.sortBy(_.index).map { db =>
        val sig = fundingSigs(db.outPoint)
        (db.outPoint, sig)
      }

      updatedRefundSigsDb = refundSigsDb.copy(initiatorSig =
        Some(cetSigs.refundSig))
      _ <- dlcRefundSigDAO.update(updatedRefundSigsDb)

      _ <- updateDLCState(dlc.contractIdOpt.get, DLCState.Signed)
      _ = logger.info(s"DLC ${contractId.toHex} is signed")
    } yield DLCSign(cetSigs, FundingSignatures(sortedSigVec), contractId)
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
    if (inputs.count(_.isInitiator) == sign.fundingSigs.length) {
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
      inputs <- dlcInputsDAO.findByDLCId(dlc.dlcId).map(_.sortBy(_.index))

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
        case Some(db) =>
          require(!db.isInitiator, "Cannot add DLC sigs as initiator")
          Future.successful(db)
        case None =>
          Future.failed(new RuntimeException(
            s"No DLC found with corresponding contractId ${contractId.toHex}"))
      }
      (_, contractData, dlcOffer, dlcAccept, fundingInputs, contractInfo) <-
        getDLCFundingData(dlcDb.dlcId)
      builder <- builderFromDbData(dlcDb,
                                   contractData,
                                   dlcOffer,
                                   dlcAccept,
                                   fundingInputs,
                                   contractInfo)

      sign = DLCSign.fromTLV(signTLV, builder.offer)
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
        require(!dlc.isInitiator, "Cannot add DLC sigs as initiator")
        dlc.state match {
          case Offered =>
            Future.failed(
              new RuntimeException(
                "Cannot add sigs to a DLC before it has been accepted"))
          case Accepted =>
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

              refundSigsDb <- dlcRefundSigDAO.findByDLCId(dlc.dlcId).map(_.get)
              sigsDbs <- dlcSigsDAO.findByDLCId(dlc.dlcId)

              updatedRefund = refundSigsDb.copy(initiatorSig =
                Some(sign.cetSigs.refundSig))
              updatedSigsDbs = sigsDbs
                .sortBy(_.index)
                .zip(sign.cetSigs.outcomeSigs)
                .map { case (db, (_, sig)) =>
                  db.copy(initiatorSig = Some(sig))
                }

              _ = logger.info(
                s"CET Signatures are valid for contract ${sign.contractId.toHex}")

              _ <- addFundingSigs(sign)
              _ <- dlcSigsDAO.updateAll(updatedSigsDbs)
              _ <- dlcRefundSigDAO.update(updatedRefund)
              updated <- dlcDAO.update(dlc.updateState(DLCState.Signed))
              _ = logger.info(
                s"DLC ${sign.contractId.toHex} sigs are verified and stored, ready to broadcast")
            } yield updated
          case _: DLCState.ClosedState | Broadcasted | Confirmed | Signed =>
            logger.info(
              s"DLC sigs already added for ${sign.contractId.toHex}, skipping..")
            Future.successful(dlc)
        }
      case None =>
        Future.failed(new NoSuchElementException(
          s"No DLC found with corresponding contractId ${sign.contractId.toHex}"))
    }
  }

  override def getDLCFundingTx(contractId: ByteVector): Future[Transaction] = {
    for {
      (dlcDb, contractData, dlcOffer, dlcAccept, fundingInputs, contractInfo) <-
        getDLCFundingData(contractId)

      signer <- signerFromDb(dlcDb,
                             contractData,
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
    val dlcDbOptF = dlcDAO.findByContractId(contractId)
    val fundingTxF = getDLCFundingTx(contractId)
    for {
      dlcDbOpt <- dlcDbOptF
      _ = dlcDbOpt match {
        case None =>
          sys.error(
            s"Cannot broadcast DLC when we don't know the contract, given contractId=${contractId}")
        case Some(dlcDb) =>
          isValidBroadcastState(dlcDb)
      }
      tx <- fundingTxF
      _ <- updateDLCState(contractId, DLCState.Broadcasted)
      _ = logger.info(
        s"Broadcasting funding transaction ${tx.txIdBE.hex} for contract ${contractId.toHex}")
      _ <- broadcastTransaction(tx)
    } yield tx
  }

  /** Checks if the DLC is in a valid state to broadcast the funding tx.
    * This is particurarily useful for situations when users want to
    * re-broadcast funding txs. You should only be able to re-broadcast
    * a funding tx in two states, [[DLCState.Signed]] or [[DLCState.Broadcasted]]
    * The reason accepted is needed is that is the state the DLC is in
    * when a user gives us their sign message
    */
  private def isValidBroadcastState(dlcDb: DLCDb): DLCDb = {
    dlcDb.state match {
      case DLCState.Broadcasted | DLCState.Signed => dlcDb
      case state @ (DLCState.Offered | DLCState.Confirmed | DLCState.Accepted |
          DLCState.Claimed | DLCState.RemoteClaimed | DLCState.Refunded) =>
        sys.error(
          s"Cannot broadcast the dlc when it is in the state=${state} contractId=${dlcDb.contractIdOpt}")
    }
  }

  override def executeDLC(
      contractId: ByteVector,
      sigs: Seq[OracleAttestmentTLV]): Future[Transaction] = {
    require(sigs.nonEmpty, "Must provide at least one oracle signature")

    for {
      dlcDb <- dlcDAO.findByContractId(contractId).map(_.get)

      (announcements, announcementData, nonceDbs) <- getDLCAnnouncementDbs(
        dlcDb.dlcId)

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
    dlcDAO.findByContractId(contractId).flatMap {
      case None =>
        Future.failed(
          new IllegalArgumentException(
            s"No DLC found with contractId $contractId"))
      case Some(db) =>
        db.closingTxIdOpt match {
          case Some(txId) =>
            transactionDAO.findByTxId(txId).flatMap {
              case Some(tx) => Future.successful(tx.transaction)
              case None     => createDLCExecutionTx(contractId, oracleSigs)
            }
          case None =>
            createDLCExecutionTx(contractId, oracleSigs)
        }
    }
  }

  private def createDLCExecutionTx(
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
      contractData <- contractDataDAO.read(dlcDb.dlcId).map(_.get)

      currentHeight <- chainQueryApi.getBestHashBlockHeight()
      _ = contractData.contractTimeout match {
        case BlockStamp.BlockHeight(height) =>
          require(
            currentHeight >= height,
            s"Refund transaction is not valid yet, current height: $currentHeight, refund valid at height $height")
        case BlockStamp.BlockTime(time) =>
          val currentTime = TimeUtil.currentEpochSecond
          require(
            currentTime >= time.toLong,
            s"Refund transaction is not valid yet, current time: $currentTime, refund valid at time $time")
      }

      executor <- executorFromDb(dlcDb.dlcId)
      refundSigsDbOpt <- dlcRefundSigDAO.findByDLCId(dlcDb.dlcId)

      refundSig =
        if (dlcDb.isInitiator) refundSigsDbOpt.get.accepterSig
        else refundSigsDbOpt.get.initiatorSig.get

      refundTx = executor.executeRefundDLC(refundSig).refundTx
      _ = logger.info(
        s"Created DLC refund transaction ${refundTx.txIdBE.hex} for contract ${contractId.toHex}")

      _ <- updateDLCState(contractId, DLCState.Refunded)
      _ <- updateClosingTxId(contractId, refundTx.txIdBE)

      _ <- processTransaction(refundTx, blockHashOpt = None)
    } yield refundTx
  }

  override def getWalletAccounting(): Future[DLCWalletAccounting] = {
    val dlcsF = listDLCs()
    for {
      dlcs <- dlcsF
      closed = dlcs.collect { case c: ClaimedDLCStatus =>
        c
      } //only get claimed dlcs for accounting
      accountings = closed.map(_.accounting)
      walletAccounting = DLCWalletAccounting.fromDLCAccounting(accountings)
    } yield walletAccounting
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

  private def getClosingTxOpt(dlcDb: DLCDb): Future[Option[TransactionDb]] = {
    val result =
      dlcDb.closingTxIdOpt.map(txid => transactionDAO.findByTxId(txid))
    result match {
      case None    => Future.successful(None)
      case Some(r) => r
    }
  }

  override def findDLC(dlcId: Sha256Digest): Future[Option[DLCStatus]] = {
    val start = System.currentTimeMillis()
    val dlcDbOptF = dlcDAO.read(dlcId)
    val contractDataOptF = contractDataDAO.read(dlcId)
    val offerDbOptF = dlcOfferDAO.read(dlcId)
    val acceptDbOptF = dlcAcceptDAO.read(dlcId)
    val closingTxOptF: Future[Option[TransactionDb]] = for {
      dlcDbOpt <- dlcDbOptF
      closingTxFOpt <- {
        dlcDbOpt.map(dlcDb => getClosingTxOpt(dlcDb)) match {
          case None                 => Future.successful(None)
          case Some(closingTxIdOpt) => closingTxIdOpt
        }
      }
    } yield closingTxFOpt

    val dlcOptF: Future[Option[DLCStatus]] = for {
      dlcDbOpt <- dlcDbOptF
      contractDataOpt <- contractDataOptF
      offerDbOpt <- offerDbOptF
      acceptDbOpt <- acceptDbOptF
      closingTxOpt <- closingTxOptF
      result <- {
        (dlcDbOpt, contractDataOpt, offerDbOpt) match {
          case (Some(dlcDb), Some(contractData), Some(offerDb)) =>
            buildDLCStatus(dlcDb,
                           contractData,
                           offerDb,
                           acceptDbOpt,
                           closingTxOpt)
          case (_, _, _) => Future.successful(None)
        }
      }
    } yield result

    dlcOptF.foreach(_ =>
      logger.debug(
        s"Done finding dlc=$dlcId, it took=${System.currentTimeMillis() - start}ms"))
    dlcOptF
  }

  /** Helper method to assemble a [[DLCStatus]] */
  private def buildDLCStatus(
      dlcDb: DLCDb,
      contractData: DLCContractDataDb,
      offerDb: DLCOfferDb,
      acceptDbOpt: Option[DLCAcceptDb],
      closingTxOpt: Option[TransactionDb]): Future[Option[DLCStatus]] = {
    val dlcId = dlcDb.dlcId
    val aggregatedF: Future[(
        Vector[DLCAnnouncementDb],
        Vector[OracleAnnouncementDataDb],
        Vector[OracleNonceDb])] = getDLCAnnouncementDbs(dlcDb.dlcId)

    val contractInfoAndAnnouncementsF: Future[
      (ContractInfo, Vector[(OracleAnnouncementV0TLV, Long)])] = {
      aggregatedF.map { case (announcements, announcementData, nonceDbs) =>
        val contractInfo = getContractInfo(contractData,
                                           announcements,
                                           announcementData,
                                           nonceDbs)
        val announcementsWithId = getOracleAnnouncementsWithId(announcements,
                                                               announcementData,
                                                               nonceDbs)
        (contractInfo, announcementsWithId)
      }
    }

    val statusF: Future[DLCStatus] = for {
      (contractInfo, announcementsWithId) <- contractInfoAndAnnouncementsF
      (announcementIds, _, nonceDbs) <- aggregatedF
      status <- {
        dlcDb.state match {
          case _: DLCState.InProgressState =>
            val inProgress = DLCStatusBuilder.buildInProgressDLCStatus(
              dlcDb = dlcDb,
              contractInfo = contractInfo,
              contractData = contractData,
              offerDb = offerDb)
            Future.successful(inProgress)
          case _: DLCState.ClosedState =>
            (acceptDbOpt, closingTxOpt) match {
              case (Some(acceptDb), Some(closingTx)) =>
                val status = DLCStatusBuilder.buildClosedDLCStatus(
                  dlcDb = dlcDb,
                  contractInfo = contractInfo,
                  contractData = contractData,
                  announcementsWithId = announcementsWithId,
                  announcementIds = announcementIds,
                  nonceDbs = nonceDbs,
                  offerDb = offerDb,
                  acceptDb = acceptDb,
                  closingTx = closingTx.transaction
                )
                Future.successful(status)
              case (None, None) =>
                Future.failed(new RuntimeException(
                  s"Could not find acceptDb or closingTx for closing state=${dlcDb.state} dlcId=$dlcId"))
              case (Some(_), None) =>
                Future.failed(new RuntimeException(
                  s"Could not find closingTx for state=${dlcDb.state} dlcId=$dlcId"))
              case (None, Some(_)) =>
                Future.failed(new RuntimeException(
                  s"Cannot find acceptDb for dlcId=$dlcId. This likely means we have data corruption"))
            }
        }
      }
    } yield status

    statusF.map(Some(_))
  }

  /** @param newAnnouncements announcements we do not have in our db
    * @param existingAnnouncements announcements we already have in our db
    */
  private case class AnnouncementGrouping(
      newAnnouncements: Vector[OracleAnnouncementDataDb],
      existingAnnouncements: Vector[OracleAnnouncementDataDb]) {
    require(existingAnnouncements.forall(_.id.isDefined))
    require(newAnnouncements.forall(_.id.isEmpty),
            s"announcmeent had id defined=${newAnnouncements.map(_.id)}")
  }

  /** This is needed because our upserts do not work
    * we need to filter announcements we already have in the database
    * to avoid issues below
    * @see https://github.com/bitcoin-s/bitcoin-s/issues/1623
    * @see https://github.com/bitcoin-s/bitcoin-s/issues/3127
    * @param announcementDataDbs
    */
  private def groupByExistingAnnouncements(
      announcementTLVs: Vector[OracleAnnouncementTLV]): Future[
    AnnouncementGrouping] = {

    val announcementSignatures: Vector[SchnorrDigitalSignature] = {
      announcementTLVs.map(a => a.announcementSignature)
    }

    val existingAnnouncementsInDbF: Future[Vector[OracleAnnouncementDataDb]] =
      announcementDAO
        .findByAnnouncementSignatures(announcementSignatures)

    for {
      existingAnnouncementsDb <- existingAnnouncementsInDbF
      newAnnouncements = announcementTLVs.filterNot(a =>
        existingAnnouncementsDb.exists(
          _.announcementSignature == a.announcementSignature))
    } yield {
      val newAnnouncementsDb =
        OracleAnnouncementDbHelper.fromAnnouncements(newAnnouncements)

      AnnouncementGrouping(newAnnouncements = newAnnouncementsDb,
                           existingAnnouncements = existingAnnouncementsDb)
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
