package org.bitcoins.dlc.wallet.internal

import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models._
import scodec.bits._

import scala.concurrent._

trait DLCDataManagement { self: DLCWallet =>

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
              val eventTLV = OracleEventV0TLV(nonces,
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
      nonceDbs: Vector[OracleNonceDb]): Vector[OracleAnnouncementV0TLV] = {
    getOracleAnnouncementsWithId(announcementIds, announcementData, nonceDbs)
      .map(_._1)
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
            val eventTLV = OracleEventV0TLV(nonces,
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
      dlcDb: DLCDb,
      announcementIds: Vector[DLCAnnouncementDb],
      announcementData: Vector[OracleAnnouncementDataDb],
      nonceDbs: Vector[OracleNonceDb]): ContractInfo = {

    val announcementTLVs =
      getOracleAnnouncements(announcementIds, announcementData, nonceDbs)

    ContractDescriptor.fromTLV(dlcDb.contractDescriptorTLV) match {
      case enum: EnumContractDescriptor =>
        val oracleInfo =
          if (dlcDb.oracleThreshold == 1 && announcementTLVs.size == 1) {
            EnumSingleOracleInfo(announcementTLVs.head)
          } else {
            EnumMultiOracleInfo(dlcDb.oracleThreshold, announcementTLVs)
          }
        ContractInfo(enum, oracleInfo)
      case numeric: NumericContractDescriptor =>
        val oracleInfo =
          if (dlcDb.oracleThreshold == 1 && announcementTLVs.size == 1) {
            NumericSingleOracleInfo(announcementTLVs.head)
          } else {
            dlcDb.oracleParamsTLVOpt match {
              case Some(params) =>
                NumericMultiOracleInfo(dlcDb.oracleThreshold,
                                       announcementTLVs,
                                       params)
              case None =>
                NumericExactMultiOracleInfo(dlcDb.oracleThreshold,
                                            announcementTLVs)
            }
          }
        ContractInfo(dlcDb.totalCollateral.satoshis, numeric, oracleInfo)
    }
  }

  private[wallet] def getDLCFundingData(contractId: ByteVector): Future[
    (
        DLCDb,
        DLCOfferDb,
        DLCAcceptDb,
        Vector[DLCFundingInputDb],
        ContractInfo)] = {
    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb = dlcDbOpt.get
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, contractInfo) <-
        getDLCFundingData(dlcDb.dlcId)
    } yield (dlcDb, dlcOffer, dlcAccept, fundingInputs, contractInfo)
  }

  private[wallet] def getDLCOfferData(dlcId: Sha256Digest): Future[
    (DLCDb, DLCOfferDb, Vector[DLCFundingInputDb], ContractInfo)] = {
    for {
      dlcDbOpt <- dlcDAO.findByDLCId(dlcId)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByDLCId(dlcId)
      dlcOffer = dlcOfferOpt.get
      fundingInputs <- dlcInputsDAO.findByDLCId(dlcId)

      announcements <- dlcAnnouncementDAO.findByDLCId(dlcDb.dlcId)
      announcementIds = announcements.map(_.announcementId)
      announcementData <- announcementDAO.findByIds(announcementIds)
      nonceDbs <- oracleNonceDAO.findByAnnouncementIds(announcementIds)

      contractInfo = getContractInfo(dlcDb,
                                     announcements,
                                     announcementData,
                                     nonceDbs)
    } yield (dlcDb, dlcOffer, fundingInputs, contractInfo)
  }

  private[wallet] def getDLCFundingData(dlcId: Sha256Digest): Future[
    (
        DLCDb,
        DLCOfferDb,
        DLCAcceptDb,
        Vector[DLCFundingInputDb],
        ContractInfo)] = {
    for {
      (dlcDb, dlcOffer, fundingInputs, contractInfo) <- getDLCOfferData(dlcId)
      dlcAcceptOpt <- dlcAcceptDAO.findByDLCId(dlcId)
      dlcAccept = dlcAcceptOpt.get
    } yield (dlcDb, dlcOffer, dlcAccept, fundingInputs, contractInfo)
  }

  private[wallet] def getAllDLCData(contractId: ByteVector): Future[
    (
        DLCDb,
        DLCOfferDb,
        DLCAcceptDb,
        DLCRefundSigDb,
        ContractInfo,
        Vector[DLCFundingInputDb],
        Vector[DLCCETSignatureDb])] = {
    for {
      dlcDbOpt <- dlcDAO.findByContractId(contractId)
      dlcDb = dlcDbOpt.get
      (_,
       dlcOffer,
       dlcAccept,
       refundSig,
       contractInfo,
       fundingInputs,
       outcomeSigs) <-
        getAllDLCData(dlcDb.dlcId)
    } yield (dlcDb,
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
        DLCOfferDb,
        DLCAcceptDb,
        DLCRefundSigDb,
        ContractInfo,
        Vector[DLCFundingInputDb],
        Vector[DLCCETSignatureDb])] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, contractInfo) <-
        getDLCFundingData(dlcId)
      refundSig <- dlcRefundSigDAO.findByDLCId(dlcId)
      outcomeSigs <- dlcSigsDAO.findByDLCId(dlcId)
    } yield (dlcDb,
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

}
