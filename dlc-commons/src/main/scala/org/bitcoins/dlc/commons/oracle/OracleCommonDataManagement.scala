package org.bitcoins.dlc.commons.oracle

import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlcoracle.db.EventOutcomeDb
import org.bitcoins.core.dlc.oracle.util.EventDbUtil
import org.bitcoins.core.dlc.oracle.{
  NonceSignaturePairDb,
  OracleAnnouncementDbHelper,
  OracleAnnouncementWithId,
  OracleMetadataDb,
  OracleMetadataDbHelper,
  OracleMetadataWithId
}
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv.{OracleAnnouncementV1TLV, OracleMetadata}
import org.bitcoins.crypto.{SchnorrNonce, SchnorrPublicKey}
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.{ExecutionContext, Future}

trait OracleCommonDataManagement extends Logging {

  protected val oracleAnnouncementDAO: OracleAnnouncementDataDAO
  protected val oracleMetadataDAO: OracleMetadataDAO
  protected val oracleSchnorrNonceDAO: OracleSchnorrNonceDAO
  protected val eventOutcomeDAO: EventOutcomeDAO

  private lazy val safeDatabase = oracleMetadataDAO.safeDatabase

  def createAnnouncementAction(annV1: OracleAnnouncementV1TLV)(implicit
      ec: ExecutionContext): DBIOAction[
    OracleAnnouncementWithId,
    NoStream,
    Effect.Write] = {
    val annDb = OracleAnnouncementDbHelper.fromAnnouncement(annV1)
    for {
      annDb <- oracleAnnouncementDAO.createAction(annDb)
      _ <- createOracleMetaDataAction(annV1.metadata, annDb.id.get)
      _ <- createEventOutcomesAction(annV1)
    } yield OracleAnnouncementWithId(annDb.id.get, annV1)
  }

  private def createEventOutcomesAction(
      announcementV1TLV: OracleAnnouncementV1TLV): DBIOAction[
    Vector[EventOutcomeDb],
    NoStream,
    Effect.Write] = {
    val outcomes: Vector[EventOutcomeDb] = EventDbUtil.toEventOutcomeDbs(
      announcementV1TLV,
      SigningVersion.latest
    )
    eventOutcomeDAO.createAllAction(outcomes)
  }

  def createAnnouncement(annV1: OracleAnnouncementV1TLV)(implicit
      ec: ExecutionContext): Future[OracleAnnouncementWithId] = {
    val action = createAnnouncementAction(annV1)
    safeDatabase.run(action)
  }

  def getOracleMetadataAction(announcementId: Long)(implicit
      ec: ExecutionContext): DBIOAction[
    Option[OracleMetadataWithId],
    NoStream,
    Effect.Read] = {
    val action = for {
      metadataDbOpt <- oracleMetadataDAO.findByPrimaryKeyAction(announcementId)
      nonceSignaturesDbs <- oracleSchnorrNonceDAO.findByIdAction(announcementId)
    } yield {
      metadataDbOpt.map { metadataDb =>
        OracleMetadata.fromDbs(metadataDb, nonceSignaturesDbs)
      }
    }
    action
  }

  def getOracleMetadata(announcementId: Long)(implicit
      ec: ExecutionContext): Future[Option[OracleMetadataWithId]] = {
    val action = getOracleMetadataAction(announcementId)
    safeDatabase.run(action)
  }

  def getAnnouncementAction(announcementId: Long)(implicit
      ec: ExecutionContext): DBIOAction[
    Option[OracleAnnouncementWithId],
    NoStream,
    Effect.Read] = {
    val metadataWithIdOptA = getOracleMetadataAction(announcementId)
    val announcementOptA = oracleAnnouncementDAO.findByIdAction(announcementId)
    for {
      metadataWithIdOpt <- metadataWithIdOptA
      announcementOpt <- announcementOptA
    } yield {
      (announcementOpt, metadataWithIdOpt) match {
        case (Some(ann), Some(metadataWithId)) =>
          val announcement =
            OracleAnnouncementV1TLV.fromAnnouncementAndMetadataDbs(
              ann,
              metadataWithId.metadata)
          val result = OracleAnnouncementWithId(announcementId, announcement)
          Some(result)
        case (None, None) => None
        case (Some(ann), None) =>
          sys.error(
            s"Cannot have v1 announcement without metadata, announcementId=$announcementId announcement=$ann")
        case (None, Some(metadata)) =>
          sys.error(
            s"Cannot have metadata without announcement, announcementId=$announcementId metadata=$metadata")
      }
    }
  }

  def getAnnouncement(announcementId: Long)(implicit
      ec: ExecutionContext): Future[Option[OracleAnnouncementWithId]] = {
    val action = getAnnouncementAction(announcementId)
    safeDatabase.run(action)
  }

  def createOracleMetaDataAction(
      oracleMetaData: OracleMetadata,
      announcementId: Long)(implicit ec: ExecutionContext): DBIOAction[
    (OracleMetadataDb, Vector[NonceSignaturePairDb]),
    NoStream,
    Effect.Write] = {
    val metdataDb =
      OracleMetadataDbHelper.fromOracleMetadata(oracleMetaData, announcementId)

    val createMetaDataA = oracleMetadataDAO.createAction(metdataDb)

    val combinedA = for {
      metadataDbWithId <- createMetaDataA
      id = metadataDbWithId.announcementId
      nonceSigPairs = oracleMetaData.nonceSignatures
      withIds = nonceSigPairs.map(p =>
        NonceSignaturePairDb(id, p.nonce, p.nonceProof, None, None))
      nonceSignatureDbs <- oracleSchnorrNonceDAO.createAllAction(withIds)
    } yield (metadataDbWithId, nonceSignatureDbs)

    combinedA
  }

  def createOracleMetadata(
      oracleMetaData: OracleMetadata,
      announcementId: Long)(implicit ec: ExecutionContext): Future[
    (OracleMetadataDb, Vector[NonceSignaturePairDb])] = {
    val action = createOracleMetaDataAction(oracleMetaData, announcementId)
    safeDatabase.run(action)
  }

  def findMetadataByAttestationPubKeyAction(
      attestationPubKey: SchnorrPublicKey)(implicit
      ec: ExecutionContext): DBIOAction[
    Vector[OracleMetadataWithId],
    NoStream,
    Effect.Read] = {
    val metadataOptA =
      oracleMetadataDAO.findByAttestationPubKeyAction(attestationPubKey)
    for {
      metadataDbs <- metadataOptA
      metdataActionVec = {
        metadataDbs.map { metadata =>
          oracleSchnorrNonceDAO
            .findByIdAction(metadata.announcementId)
            .map(nonces => OracleMetadata.fromDbs(metadata, nonces))
        }
      }
      metadataVec <- DBIOAction.sequence(metdataActionVec)
    } yield metadataVec
  }

  def findMetadataByAttestationPubKey(attestationPubKey: SchnorrPublicKey)(
      implicit ec: ExecutionContext): Future[Vector[OracleMetadataWithId]] = {
    val action = findMetadataByAttestationPubKeyAction(attestationPubKey)
    safeDatabase.run(action)
  }

  def findMetadataByNonce(nonce: SchnorrNonce)(implicit
      ec: ExecutionContext): Future[Option[OracleMetadataWithId]] = {

    for {
      nonceOpt <- oracleSchnorrNonceDAO.findByNonce(nonce)
      metadataOpt <- {
        nonceOpt match {
          case Some(db) => getOracleMetadata(db.announcementId)
          case None     => Future.successful(None)
        }
      }
    } yield metadataOpt
  }

  def getNonce(nonce: SchnorrNonce): Future[Option[NonceSignaturePairDb]] = {
    oracleSchnorrNonceDAO.findByNonce(nonce)
  }

  def getAnnouncmementByNonce(nonce: SchnorrNonce)(implicit
      ec: ExecutionContext): Future[
    Option[(OracleAnnouncementV1TLV, NonceSignaturePairDb)]] = {
    val nonceV1OptF = getNonce(nonce)
    val announcementNoncePairOptF: Future[
      Option[(OracleAnnouncementV1TLV, NonceSignaturePairDb)]] = {
      nonceV1OptF.flatMap {
        case Some(nonce) =>
          getAnnouncement(nonce.announcementId)
            .map {
              case Some(ann) =>
                Some(
                  (ann.announcement.asInstanceOf[OracleAnnouncementV1TLV],
                   nonce))
              case None => None
            }
        case None => Future.successful(None)
      }
    }

    announcementNoncePairOptF
  }

  def getOutcomesForNonceAction(nonce: SchnorrNonce): DBIOAction[
    Vector[EventOutcomeDb],
    NoStream,
    Effect.Read] = {
    eventOutcomeDAO.findByNonceAction(nonce)
  }
}
