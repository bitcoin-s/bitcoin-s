package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.execution._
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.dlc.sign.DLCTxSigner
import org.bitcoins.core.protocol.dlc.verify.DLCSignatureVerifier
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted.{OrderedAnnouncements, OrderedNonces}
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.db.SafeDatabase
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.dlc.wallet.util.{DLCActionBuilder, DLCTxUtil}
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.wallet.models.TransactionDAO
import scodec.bits._
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent._

/** Handles fetching and constructing different DLC datastructures from the database */
case class DLCDataManagement(dlcWalletDAOs: DLCWalletDAOs)(implicit
    ec: ExecutionContext) {
  val dlcDAO = dlcWalletDAOs.dlcDAO
  private val dlcAnnouncementDAO = dlcWalletDAOs.dlcAnnouncementDAO
  //private val dlcInputsDAO = dlcWalletDAOs.dlcInputsDAO
  //private val dlcOfferDAO = dlcWalletDAOs.dlcOfferDAO
  private val contractDataDAO = dlcWalletDAOs.contractDataDAO
  private val dlcAcceptDAO = dlcWalletDAOs.dlcAcceptDAO
  private val dlcSigsDAO = dlcWalletDAOs.dlcSigsDAO
  private val dlcRefundSigDAO = dlcWalletDAOs.dlcRefundSigDAO
  private val announcementDAO = dlcWalletDAOs.oracleAnnouncementDAO
  private val oracleNonceDAO = dlcWalletDAOs.oracleNonceDAO
  private val remoteTxDAO = dlcWalletDAOs.dlcRemoteTxDAO

  private val actionBuilder: DLCActionBuilder = {
    DLCActionBuilder(dlcWalletDAOs)
  }
  private val safeDatabase: SafeDatabase = dlcDAO.safeDatabase

  private[wallet] def getDLCAnnouncementDbs(dlcId: Sha256Digest): Future[(
      Vector[DLCAnnouncementDb],
      Vector[OracleAnnouncementDataDb],
      Vector[OracleNonceDb])] = {
    val announcementsF = dlcAnnouncementDAO.findByDLCId(dlcId)
    val announcementIdsF: Future[Vector[Long]] = for {
      announcements <- announcementsF
      announcementIds = announcements.map(_.announcementId)
    } yield announcementIds
    val announcementDataF =
      announcementIdsF.flatMap(ids => announcementDAO.findByIds(ids))
    val noncesDbF =
      announcementIdsF.flatMap(ids => oracleNonceDAO.findByAnnouncementIds(ids))

    for {
      announcements <- announcementsF
      announcementData <- announcementDataF
      nonceDbs <- noncesDbF
    } yield {
      (announcements, announcementData, nonceDbs)
    }
  }

  /** Fetches the oracle announcements of the oracles
    * that were used for execution in a DLC
    */
  private[wallet] def getUsedOracleAnnouncements(
      dlcAnnouncementDbs: Vector[DLCAnnouncementDb],
      announcementData: Vector[OracleAnnouncementDataDb],
      nonceDbs: Vector[OracleNonceDb]): Vector[
    (OracleAnnouncementV0TLV, Long)] = {
    val withIds = nonceDbs
      .groupBy(_.announcementId)
      .toVector
      .flatMap { case (id, nonceDbs) =>
        announcementData.find(_.id.contains(id)) match {
          case Some(data) =>
            val used = dlcAnnouncementDbs
              .find(_.announcementId == data.id.get)
              .exists(_.used)
            if (used) {
              val nonces = nonceDbs.sortBy(_.index).map(_.nonce)
              val eventTLV = OracleEventV0TLV(OrderedNonces(nonces),
                                              data.eventMaturity,
                                              data.eventDescriptor,
                                              data.eventId)
              Some(
                (OracleAnnouncementV0TLV(data.announcementSignature,
                                         data.publicKey,
                                         eventTLV),
                 data.id.get))
            } else None
          case None =>
            throw new RuntimeException(s"Error no data for announcement id $id")
        }
      }
    dlcAnnouncementDbs
      .sortBy(_.index)
      .flatMap(a => withIds.find(_._2 == a.announcementId))
  }

  private[wallet] def getOracleAnnouncements(
      announcementIds: Vector[DLCAnnouncementDb],
      announcementData: Vector[OracleAnnouncementDataDb],
      nonceDbs: Vector[OracleNonceDb]): OrderedAnnouncements = {
    val announcements =
      getOracleAnnouncementsWithId(announcementIds, announcementData, nonceDbs)
        .map(_._1)
    OrderedAnnouncements(announcements)
  }

  private[wallet] def getOracleAnnouncementsWithId(
      announcementIds: Vector[DLCAnnouncementDb],
      announcementData: Vector[OracleAnnouncementDataDb],
      nonceDbs: Vector[OracleNonceDb]): Vector[
    (OracleAnnouncementV0TLV, Long)] = {
    val withIds = nonceDbs
      .groupBy(_.announcementId)
      .toVector
      .map { case (id, nonceDbs) =>
        announcementData.find(_.id.contains(id)) match {
          case Some(data) =>
            val nonces = nonceDbs.sortBy(_.index).map(_.nonce)
            val eventTLV = OracleEventV0TLV(OrderedNonces(nonces),
                                            data.eventMaturity,
                                            data.eventDescriptor,
                                            data.eventId)
            (OracleAnnouncementV0TLV(data.announcementSignature,
                                     data.publicKey,
                                     eventTLV),
             data.id.get)
          case None =>
            throw new RuntimeException(s"Error no data for announcement id $id")
        }
      }
    announcementIds
      .sortBy(_.index)
      .flatMap(a => withIds.find(_._2 == a.announcementId))
  }

  private[wallet] def getContractInfo(
      dlcId: Sha256Digest): Future[ContractInfo] = {
    for {
      contractData <- contractDataDAO.read(dlcId).map(_.get)
      (announcements, announcementData, nonceDbs) <- getDLCAnnouncementDbs(
        dlcId)
    } yield getContractInfo(contractData,
                            announcements,
                            announcementData,
                            nonceDbs)
  }

  private[wallet] def getContractInfo(
      contractDataDb: DLCContractDataDb,
      announcementIds: Vector[DLCAnnouncementDb],
      announcementData: Vector[OracleAnnouncementDataDb],
      nonceDbs: Vector[OracleNonceDb]): ContractInfo = {

    val announcementTLVs =
      getOracleAnnouncements(announcementIds, announcementData, nonceDbs)

    ContractDescriptor.fromTLV(contractDataDb.contractDescriptorTLV) match {
      case enum: EnumContractDescriptor =>
        val oracleInfo =
          if (announcementTLVs.size == 1) {
            EnumSingleOracleInfo(announcementTLVs.head)
          } else {
            EnumMultiOracleInfo(contractDataDb.oracleThreshold,
                                announcementTLVs)
          }
        SingleContractInfo(contractDataDb.totalCollateral.satoshis,
                           enum,
                           oracleInfo)
      case numeric: NumericContractDescriptor =>
        val oracleInfo =
          if (announcementTLVs.size == 1) {
            NumericSingleOracleInfo(announcementTLVs.head)
          } else {
            contractDataDb.oracleParamsTLVOpt match {
              case Some(params) =>
                NumericMultiOracleInfo(contractDataDb.oracleThreshold,
                                       announcementTLVs,
                                       params)
              case None =>
                NumericExactMultiOracleInfo(contractDataDb.oracleThreshold,
                                            announcementTLVs)
            }
          }
        SingleContractInfo(contractDataDb.totalCollateral.satoshis,
                           numeric,
                           oracleInfo)
    }
  }

  private[wallet] def getDLCFundingData(contractId: ByteVector): Future[
    (
        DLCDb,
        DLCContractDataDb,
        DLCOfferDb,
        DLCAcceptDb,
        Vector[DLCFundingInputDb],
        ContractInfo)] = {
    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb = dlcDbOpt.get
      (_, contractData, dlcOffer, dlcAccept, fundingInputs, contractInfo) <-
        getDLCFundingData(dlcDb.dlcId)
    } yield (dlcDb,
             contractData,
             dlcOffer,
             dlcAccept,
             fundingInputs,
             contractInfo)
  }

  private[wallet] def getDLCOfferData(dlcId: Sha256Digest): Future[
    (
        DLCDb,
        DLCContractDataDb,
        DLCOfferDb,
        Vector[DLCFundingInputDb],
        ContractInfo)] = {

    val combined = actionBuilder.getDLCOfferDataAction(dlcId)
    val combinedF = safeDatabase.run(combined)
    for {
      (dlcDbs, contractDataDbs, offerDbs, fundingInputDbs) <- combinedF
      dlcDb = dlcDbs.head

      contractData = contractDataDbs.head

      dlcOffer = offerDbs.head

      (announcements, announcementData, nonceDbs) <- getDLCAnnouncementDbs(
        dlcId)

      contractInfo = getContractInfo(contractData,
                                     announcements,
                                     announcementData,
                                     nonceDbs)

      sortedInputs = fundingInputDbs.sortBy(_.index)
    } yield {
      (dlcDb, contractData, dlcOffer, sortedInputs, contractInfo)
    }
  }

  private[wallet] def getDLCFundingData(dlcId: Sha256Digest): Future[
    (
        DLCDb,
        DLCContractDataDb,
        DLCOfferDb,
        DLCAcceptDb,
        Vector[DLCFundingInputDb],
        ContractInfo)] = {
    for {
      (dlcDb, contractData, dlcOffer, fundingInputs, contractInfo) <-
        getDLCOfferData(dlcId)
      dlcAcceptOpt <- dlcAcceptDAO.findByDLCId(dlcId)
      dlcAccept = dlcAcceptOpt.head
    } yield (dlcDb,
             contractData,
             dlcOffer,
             dlcAccept,
             fundingInputs,
             contractInfo)
  }

  private[wallet] def getAllDLCData(contractId: ByteVector): Future[
    (
        DLCDb,
        DLCContractDataDb,
        DLCOfferDb,
        DLCAcceptDb,
        DLCRefundSigsDb,
        ContractInfo,
        Vector[DLCFundingInputDb],
        Option[Vector[DLCCETSignaturesDb]])] = {
    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb = dlcDbOpt.get
      (_,
       contractData,
       dlcOffer,
       dlcAccept,
       refundSig,
       contractInfo,
       fundingInputs,
       outcomeSigs) <-
        getAllDLCData(dlcDb.dlcId)
    } yield (dlcDb,
             contractData,
             dlcOffer,
             dlcAccept,
             refundSig,
             contractInfo,
             fundingInputs,
             outcomeSigs)
  }

  private[wallet] def getAllDLCData(dlcId: Sha256Digest): Future[
    (
        DLCDb,
        DLCContractDataDb,
        DLCOfferDb,
        DLCAcceptDb,
        DLCRefundSigsDb,
        ContractInfo,
        Vector[DLCFundingInputDb],
        Option[Vector[DLCCETSignaturesDb]])] = {
    val safeDatabase = dlcRefundSigDAO.safeDatabase
    val refundSigDLCs = dlcRefundSigDAO.findByDLCIdAction(dlcId)
    val sigDLCs = dlcSigsDAO.findByDLCIdAction(dlcId)

    val refundAndOutcomeSigsAction =
      refundSigDLCs.flatMap(r => sigDLCs.map(s => (r, s)))
    val refundAndOutcomeSigsF = safeDatabase.run(refundAndOutcomeSigsAction)
    for {
      (dlcDb, contractData, dlcOffer, dlcAccept, fundingInputs, contractInfo) <-
        getDLCFundingData(dlcId)
      (refundSigs, outcomeSigs) <- refundAndOutcomeSigsF
    } yield {

      val sigsOpt = if (outcomeSigs.isEmpty) None else Some(outcomeSigs)

      (dlcDb,
       contractData,
       dlcOffer,
       dlcAccept,
       refundSigs.head,
       contractInfo,
       fundingInputs,
       sigsOpt)
    }
  }

  private[wallet] def verifierFromAccept(
      accept: DLCAccept,
      transactionDAO: TransactionDAO): Future[DLCSignatureVerifier] = {
    for {
      dlcDbOpt <- dlcDAO.findByTempContractId(accept.tempContractId)
      dlcDb = dlcDbOpt.get

      (_, contractData, dlcOffer, fundingInputsDb, contractInfo) <-
        getDLCOfferData(dlcDb.dlcId)
      localFundingInputs = fundingInputsDb.filter(_.isInitiator)

      prevTxs <-
        transactionDAO.findByTxIdBEs(localFundingInputs.map(_.outPoint.txIdBE))
    } yield {
      val offerFundingInputs =
        DLCTxUtil.matchPrevTxsWithInputs(localFundingInputs, prevTxs)
      val offer = dlcOffer.toDLCOffer(contractInfo,
                                      offerFundingInputs,
                                      dlcDb,
                                      contractData)

      val builder = DLCTxBuilder(offer, accept.withoutSigs)

      DLCSignatureVerifier(builder, dlcDb.isInitiator)
    }
  }

  private[wallet] def verifierFromDb(
      contractId: ByteVector,
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO): Future[DLCSignatureVerifier] = {
    getDLCFundingData(contractId).flatMap {
      case (dlcDb,
            contractData,
            dlcOffer,
            dlcAccept,
            fundingInputsDb,
            contractInfo) =>
        verifierFromDbData(
          dlcDb = dlcDb,
          contractData = contractData,
          dlcOffer = dlcOffer,
          dlcAccept = dlcAccept,
          fundingInputsDb = fundingInputsDb,
          contractInfo = contractInfo,
          transactionDAO = transactionDAO,
          remoteTxDAO = remoteTxDAO
        )
    }
  }

  def getOffer(
      dlcId: Sha256Digest,
      transactionDAO: TransactionDAO): Future[DLCOffer] = {
    val dataF = getAllDLCData(dlcId)
    dataF.flatMap {
      case (dlcDb,
            contractDataDb,
            offerDb,
            acceptDb,
            _,
            contractInfo,
            fundingInputsDb,
            _) =>
        builderFromDbData(dlcDb,
                          contractDataDb,
                          offerDb,
                          acceptDb,
                          fundingInputsDb,
                          contractInfo,
                          transactionDAO,
                          remoteTxDAO = remoteTxDAO)
          .map(_.offer)
    }
  }

  private[wallet] def builderFromDbData(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo,
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO): Future[DLCTxBuilder] = {
    val (localDbFundingInputs, remoteDbFundingInputs) = if (dlcDb.isInitiator) {
      (fundingInputsDb.filter(_.isInitiator),
       fundingInputsDb.filterNot(_.isInitiator))
    } else {
      (fundingInputsDb.filterNot(_.isInitiator),
       fundingInputsDb.filter(_.isInitiator))
    }

    for {
      localPrevTxs <- transactionDAO.findByTxIdBEs(
        localDbFundingInputs.map(_.outPoint.txIdBE))
      remotePrevTxs <-
        remoteTxDAO.findByTxIdBEs(remoteDbFundingInputs.map(_.outPoint.txIdBE))
    } yield {
      val localFundingInputs = DLCTxUtil.matchPrevTxsWithInputs(
        inputs = localDbFundingInputs,
        prevTxs = localPrevTxs)

      val remoteFundingInputs = DLCTxUtil.matchPrevTxsWithInputs(
        inputs = remoteDbFundingInputs,
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
                                      contractDataDb.dlcTimeouts)

      val accept = dlcAccept.toDLCAcceptWithoutSigs(dlcDb.tempContractId,
                                                    acceptFundingInputs)

      DLCTxBuilder(offer, accept)
    }
  }

  private[wallet] def verifierFromDbData(
      dlcDb: DLCDb,
      contractData: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo,
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO): Future[DLCSignatureVerifier] = {
    val builderF =
      builderFromDbData(
        dlcDb = dlcDb,
        contractDataDb = contractData,
        dlcOffer = dlcOffer,
        dlcAccept = dlcAccept,
        fundingInputsDb = fundingInputsDb,
        contractInfo = contractInfo,
        transactionDAO = transactionDAO,
        remoteTxDAO = remoteTxDAO
      )

    builderF.map(DLCSignatureVerifier(_, dlcDb.isInitiator))
  }

  private[wallet] def signerFromDb(
      dlcId: Sha256Digest,
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO,
      fundingUtxoScriptSigParams: Vector[ScriptSignatureParams[InputInfo]],
      keyManager: BIP39KeyManager): Future[DLCTxSigner] = {
    for {
      (dlcDb,
       contractData,
       dlcOffer,
       dlcAccept,
       fundingInputsDb,
       contractInfo) <-
        getDLCFundingData(dlcId)
      signer <- signerFromDb(
        dlcDb = dlcDb,
        contractDataDb = contractData,
        dlcOffer = dlcOffer,
        dlcAccept = dlcAccept,
        fundingInputsDb = fundingInputsDb,
        fundingUtxoScriptSigParams = fundingUtxoScriptSigParams,
        contractInfo = contractInfo,
        transactionDAO = transactionDAO,
        remoteTxDAO = remoteTxDAO,
        keyManager = keyManager
      )
    } yield signer
  }

  private[wallet] def signerFromDb(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      fundingUtxoScriptSigParams: Vector[ScriptSignatureParams[InputInfo]],
      contractInfo: ContractInfo,
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO,
      keyManager: BIP39KeyManager): Future[DLCTxSigner] = {
    for {
      builder <- builderFromDbData(
        dlcDb = dlcDb,
        contractDataDb = contractDataDb,
        dlcOffer = dlcOffer,
        dlcAccept = dlcAccept,
        fundingInputsDb = fundingInputsDb,
        contractInfo = contractInfo,
        transactionDAO = transactionDAO,
        remoteTxDAO = remoteTxDAO
      )
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
                  fundingUtxos = fundingUtxoScriptSigParams)
    }
  }

  private[wallet] def executorFromDb(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      fundingUtxoScriptSigParams: Vector[ScriptSignatureParams[InputInfo]],
      contractInfo: ContractInfo,
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO,
      keyManager: BIP39KeyManager): Future[DLCExecutor] = {
    signerFromDb(
      dlcDb = dlcDb,
      contractDataDb = contractDataDb,
      dlcOffer = dlcOffer,
      dlcAccept = dlcAccept,
      fundingInputsDb = fundingInputsDb,
      fundingUtxoScriptSigParams = fundingUtxoScriptSigParams,
      contractInfo = contractInfo,
      transactionDAO = transactionDAO,
      remoteTxDAO = remoteTxDAO,
      keyManager = keyManager
    ).map(DLCExecutor.apply)
  }

  private[wallet] def executorFromDb(
      dlcId: Sha256Digest,
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO,
      fundingUtxoScriptSigParams: Vector[ScriptSignatureParams[InputInfo]],
      keyManager: BIP39KeyManager): Future[DLCExecutor] = {
    signerFromDb(dlcId = dlcId,
                 transactionDAO = transactionDAO,
                 remoteTxDAO = remoteTxDAO,
                 fundingUtxoScriptSigParams = fundingUtxoScriptSigParams,
                 keyManager = keyManager).map(DLCExecutor.apply)
  }

  /** Builds an [[DLCExecutor]] and [[SetupDLC]] for a given contract id
    * @return the executor and setup if we still have CET signatures else return None
    */
  private[wallet] def executorAndSetupFromDb(
      contractId: ByteVector,
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO,
      fundingUtxoScriptSigParams: Vector[ScriptSignatureParams[InputInfo]],
      keyManager: BIP39KeyManager): Future[Option[DLCExecutorWithSetup]] = {
    getAllDLCData(contractId).flatMap {
      case (dlcDb,
            contractData,
            dlcOffer,
            dlcAccept,
            refundSigs,
            contractInfo,
            fundingInputsDb,
            outcomeSigsDbsOpt) =>
        outcomeSigsDbsOpt match {
          case Some(outcomeSigsDbs) =>
            executorAndSetupFromDb(
              dlcDb = dlcDb,
              contractDataDb = contractData,
              dlcOffer = dlcOffer,
              dlcAccept = dlcAccept,
              refundSigsDb = refundSigs,
              contractInfo = contractInfo,
              fundingInputs = fundingInputsDb,
              outcomeSigsDbs = outcomeSigsDbs,
              transactionDAO = transactionDAO,
              remoteTxDAO = remoteTxDAO,
              fundingUtxoScriptSigParams = fundingUtxoScriptSigParams,
              keyManager = keyManager
            ).map(Some(_))
          case None =>
            //means we cannot re-create messages because
            //we don't have the cets in the database anymore
            Future.successful(None)

        }

    }
  }

  private[wallet] def executorAndSetupFromDb(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      refundSigsDb: DLCRefundSigsDb,
      contractInfo: ContractInfo,
      fundingInputs: Vector[DLCFundingInputDb],
      outcomeSigsDbs: Vector[DLCCETSignaturesDb],
      transactionDAO: TransactionDAO,
      remoteTxDAO: DLCRemoteTxDAO,
      fundingUtxoScriptSigParams: Vector[ScriptSignatureParams[InputInfo]],
      keyManager: BIP39KeyManager): Future[DLCExecutorWithSetup] = {

    val dlcExecutorF = executorFromDb(
      dlcDb = dlcDb,
      contractDataDb = contractDataDb,
      dlcOffer = dlcOffer,
      dlcAccept = dlcAccept,
      fundingInputsDb = fundingInputs,
      fundingUtxoScriptSigParams = fundingUtxoScriptSigParams,
      contractInfo = contractInfo,
      transactionDAO = transactionDAO,
      remoteTxDAO = remoteTxDAO,
      keyManager = keyManager
    )

    dlcExecutorF.flatMap { executor =>
      // Filter for only counterparty's outcome sigs
      val outcomeSigs = if (dlcDb.isInitiator) {
        outcomeSigsDbs
          .map { dbSig =>
            dbSig.sigPoint -> dbSig.accepterSig
          }
      } else {
        outcomeSigsDbs
          .map { dbSig =>
            dbSig.sigPoint -> dbSig.initiatorSig.get
          }
      }
      val refundSig = if (dlcDb.isInitiator) {
        refundSigsDb.accepterSig
      } else refundSigsDb.initiatorSig.get

      //sometimes we do not have cet signatures, for instance
      //if we have settled a DLC, we prune the cet signatures
      //from the database
      val cetSigs = CETSignatures(outcomeSigs)

      val setupF = if (dlcDb.isInitiator) {
        // Note that the funding tx in this setup is not signed
        executor.setupDLCOffer(cetSigs, refundSig)
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
        executor.setupDLCAccept(cetSigs,
                                refundSig,
                                FundingSignatures(fundingSigs),
                                None)
      }

      Future.fromTry(setupF.map(DLCExecutorWithSetup(executor, _)))
    }
  }

  def getCetAndRefundSigsAction(dlcId: Sha256Digest): DBIOAction[
    (Vector[DLCCETSignaturesDb], Option[DLCRefundSigsDb]),
    NoStream,
    Effect.Read] = {
    val cetSigsAction = dlcSigsDAO.findByDLCIdAction(dlcId)
    val refundSigsAction = dlcRefundSigDAO.findByDLCIdAction(dlcId)
    for {
      cetSigs <- cetSigsAction
      refundSigs <- refundSigsAction
    } yield (cetSigs, refundSigs)
  }

  def getCetAndRefundSigs(dlcId: Sha256Digest): Future[
    (Vector[DLCCETSignaturesDb], Option[DLCRefundSigsDb])] = {
    val action = getCetAndRefundSigsAction(dlcId)
    safeDatabase.run(action)
  }
}

object DLCDataManagement {

  def fromDbAppConfig()(implicit
      dbAppConfig: DLCAppConfig,
      ec: ExecutionContext): DLCDataManagement = {
    val announcementDAO: OracleAnnouncementDataDAO =
      OracleAnnouncementDataDAO()
    val oracleNonceDAO: OracleNonceDAO = OracleNonceDAO()

    val dlcAnnouncementDAO: DLCAnnouncementDAO =
      DLCAnnouncementDAO()
    val dlcOfferDAO: DLCOfferDAO = DLCOfferDAO()
    val dlcAcceptDAO: DLCAcceptDAO = DLCAcceptDAO()
    val dlcDAO: DLCDAO = DLCDAO()

    val contractDataDAO: DLCContractDataDAO =
      DLCContractDataDAO()
    val dlcInputsDAO: DLCFundingInputDAO = DLCFundingInputDAO()
    val dlcSigsDAO: DLCCETSignaturesDAO = DLCCETSignaturesDAO()
    val dlcRefundSigDAO: DLCRefundSigsDAO = DLCRefundSigsDAO()
    val dlcRemoteTxDAO: DLCRemoteTxDAO = DLCRemoteTxDAO()

    val dlcWalletDAOs = DLCWalletDAOs(
      dlcDAO,
      contractDataDAO,
      dlcAnnouncementDAO,
      dlcInputsDAO,
      dlcOfferDAO,
      dlcAcceptDAO,
      dlcSigsDAO,
      dlcRefundSigDAO,
      oracleNonceDAO,
      announcementDAO,
      dlcRemoteTxDAO
    )

    DLCDataManagement(dlcWalletDAOs)
  }
}
