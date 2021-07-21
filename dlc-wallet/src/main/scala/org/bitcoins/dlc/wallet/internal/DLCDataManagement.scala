package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.wallet.db.TransactionDb
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.execution._
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.dlc.sign.DLCTxSigner
import org.bitcoins.core.protocol.dlc.verify.DLCSignatureVerifier
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.util.sorted.{OrderedAnnouncements, OrderedNonces}
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models._
import scodec.bits._

import scala.concurrent._

/** Handles fetching and constructing different DLC datastructures from the database */
private[bitcoins] trait DLCDataManagement { self: DLCWallet =>

  private[wallet] def getDLCAnnouncementDbs(dlcId: Sha256Digest): Future[(
      Vector[DLCAnnouncementDb],
      Vector[OracleAnnouncementDataDb],
      Vector[OracleNonceDb])] = {
    val announcementsF = dlcAnnouncementDAO.findByDLCId(dlcId)
    val announcementIdsF = for {
      announcements <- announcementsF
      announcementIds = announcements.map(_.announcementId)
    } yield announcementIds

    val announcementDataF =
      announcementIdsF.flatMap(ids => announcementDAO.findByIds(ids))
    val nonceDbsF =
      announcementIdsF.flatMap(ids => oracleNonceDAO.findByAnnouncementIds(ids))

    for {
      announcements <- announcementsF
      announcementData <- announcementDataF
      nonceDbs <- nonceDbsF
    } yield (announcements, announcementData, nonceDbs)
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
        ContractInfo(contractDataDb.totalCollateral.satoshis, enum, oracleInfo)
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
        ContractInfo(contractDataDb.totalCollateral.satoshis,
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
    for {
      dlcDbOpt <- dlcDAO.findByDLCId(dlcId)
      dlcDb = dlcDbOpt.get
      contractDataOpt <- contractDataDAO.findByDLCId(dlcId)
      contractData = contractDataOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByDLCId(dlcId)
      dlcOffer = dlcOfferOpt.get
      fundingInputs <- dlcInputsDAO.findByDLCId(dlcId)

      (announcements, announcementData, nonceDbs) <- getDLCAnnouncementDbs(
        dlcId)

      contractInfo = getContractInfo(contractData,
                                     announcements,
                                     announcementData,
                                     nonceDbs)
    } yield (dlcDb, contractData, dlcOffer, fundingInputs, contractInfo)
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
      dlcAccept = dlcAcceptOpt.get
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
        Vector[DLCCETSignaturesDb])] = {
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
        Vector[DLCCETSignaturesDb])] = {
    for {
      (dlcDb, contractData, dlcOffer, dlcAccept, fundingInputs, contractInfo) <-
        getDLCFundingData(dlcId)
      refundSig <- dlcRefundSigDAO.findByDLCId(dlcId)
      outcomeSigs <- dlcSigsDAO.findByDLCId(dlcId)
    } yield (dlcDb,
             contractData,
             dlcOffer,
             dlcAccept,
             refundSig.get,
             contractInfo,
             fundingInputs,
             outcomeSigs)
  }

  private[wallet] def fundingUtxosFromDb(
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

  private[wallet] def verifierFromAccept(
      accept: DLCAccept): Future[DLCSignatureVerifier] = {
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
        matchPrevTxsWithInputs(localFundingInputs, prevTxs)

      val builder =
        DLCTxBuilder(dlcOffer.toDLCOffer(contractInfo,
                                         offerFundingInputs,
                                         dlcDb,
                                         contractData),
                     accept.withoutSigs)

      DLCSignatureVerifier(builder, dlcDb.isInitiator)
    }
  }

  private[wallet] def verifierFromDb(
      contractId: ByteVector): Future[DLCSignatureVerifier] = {
    getDLCFundingData(contractId).flatMap {
      case (dlcDb,
            contractData,
            dlcOffer,
            dlcAccept,
            fundingInputsDb,
            contractInfo) =>
        verifierFromDbData(dlcDb,
                           contractData,
                           dlcOffer,
                           dlcAccept,
                           fundingInputsDb,
                           contractInfo)
    }
  }

  private[wallet] def builderFromDbData(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo): Future[DLCTxBuilder] = {
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
                                      contractDataDb.dlcTimeouts)

      val accept = dlcAccept.toDLCAcceptWithoutSigs(dlcDb.tempContractId,
                                                    acceptFundingInputs)

      DLCTxBuilder(offer, accept)
    }
  }

  /** Takes in a list of inputs to fund DLCs, and pairs them with the full funding transaction for this input
    * and then converts the input tx pair to a [[DLCFundingInput]]
    * @throws NoSuchElementException when we have an input we cannot find the funding transaction for
    */
  private[wallet] def matchPrevTxsWithInputs(
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

  private[wallet] def verifierFromDbData(
      dlcDb: DLCDb,
      contractData: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo): Future[DLCSignatureVerifier] = {
    val builderF =
      builderFromDbData(dlcDb,
                        contractData,
                        dlcOffer,
                        dlcAccept,
                        fundingInputsDb,
                        contractInfo)

    builderF.map(DLCSignatureVerifier(_, dlcDb.isInitiator))
  }

  private[wallet] def signerFromDb(dlcId: Sha256Digest): Future[DLCTxSigner] = {
    for {
      (dlcDb,
       contractData,
       dlcOffer,
       dlcAccept,
       fundingInputsDb,
       contractInfo) <-
        getDLCFundingData(dlcId)
      signer <- signerFromDb(dlcDb,
                             contractData,
                             dlcOffer,
                             dlcAccept,
                             fundingInputsDb,
                             contractInfo)
    } yield signer
  }

  private[wallet] def signerFromDb(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo): Future[DLCTxSigner] = {
    for {
      fundingUtxos <- fundingUtxosFromDb(dlcDb, fundingInputsDb)
      builder <- builderFromDbData(dlcDb = dlcDb,
                                   contractDataDb = contractDataDb,
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

  private[wallet] def executorFromDb(
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb],
      contractInfo: ContractInfo): Future[DLCExecutor] = {
    signerFromDb(dlcDb,
                 contractDataDb,
                 dlcOffer,
                 dlcAccept,
                 fundingInputsDb,
                 contractInfo).map(DLCExecutor.apply)
  }

  private[wallet] def executorFromDb(
      dlcId: Sha256Digest): Future[DLCExecutor] = {
    signerFromDb(dlcId).map(DLCExecutor.apply)
  }

  private[wallet] def executorAndSetupFromDb(
      contractId: ByteVector): Future[(DLCExecutor, SetupDLC)] = {
    getAllDLCData(contractId).flatMap {
      case (dlcDb,
            contractData,
            dlcOffer,
            dlcAccept,
            refundSigs,
            contractInfo,
            fundingInputsDb,
            outcomeSigsDbs) =>
        executorAndSetupFromDb(dlcDb,
                               contractData,
                               dlcOffer,
                               dlcAccept,
                               refundSigs,
                               contractInfo,
                               fundingInputsDb,
                               outcomeSigsDbs)
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
      outcomeSigsDbs: Vector[DLCCETSignaturesDb]): Future[
    (DLCExecutor, SetupDLC)] = {

    executorFromDb(dlcDb,
                   contractDataDb,
                   dlcOffer,
                   dlcAccept,
                   fundingInputs,
                   contractInfo)
      .flatMap { executor =>
        // Filter for only counter party's outcome sigs
        val outcomeSigs =
          if (dlcDb.isInitiator) {
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
}
