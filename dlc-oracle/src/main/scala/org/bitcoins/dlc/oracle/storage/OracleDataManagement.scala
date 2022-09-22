package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.api.dlcoracle.db.RValueDb
import org.bitcoins.core.dlc.oracle.{
  NonceSignaturePairDb,
  OracleAnnouncementWithId
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.{ECPrivateKey, SchnorrNonce}
import org.bitcoins.dlc.commons.oracle.{
  OracleAnnouncementDataDAO,
  OracleCommonDataManagement,
  OracleMetadataDAO,
  OracleSchnorrNonceDAO
}
import slick.dbio.{DBIOAction, Effect, NoStream}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class OracleDataManagement(daos: DLCOracleDAOs)(implicit
    ec: ExecutionContext)
    extends OracleCommonDataManagement {

  override protected val oracleAnnouncementDAO: OracleAnnouncementDataDAO =
    daos.oracleAnnouncementDAO

  override protected val oracleMetadataDAO: OracleMetadataDAO =
    daos.oracleMetadataDAO

  override protected val oracleSchnorrNonceDAO: OracleSchnorrNonceDAO =
    daos.oracleSchnorrNonceDAO

  private val eventDAO: EventDAO = daos.eventDAO

  private val rValueDAO = daos.rValueDAO

  private val safeDatabase = oracleMetadataDAO.safeDatabase

  def createNewAnnouncement(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorDLCType,
      announcementPrivKey: ECPrivateKey,
      metadata: OracleMetadata,
      rValueDbs: Vector[RValueDb],
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    OracleAnnouncementWithId] = {
    require(maturationTime.isAfter(TimeUtil.now),
            s"Event cannot mature in the past, got $maturationTime")

    for {
      dbs <- eventDAO.findByEventName(eventName)
      _ = require(dbs.isEmpty, s"Event name ($eventName) is already being used")

      epoch = UInt32(maturationTime.getEpochSecond)

      eventTLV = OracleEventV1TLV(eventDescriptor = descriptor,
                                  eventName,
                                  FixedOracleEventTimestamp(epoch))

      announcementSignature = OracleAnnouncementV1TLV
        .buildAnnouncementSignature(announcementPrivKey = announcementPrivKey,
                                    signingVersion = signingVersion,
                                    eventTLV = eventTLV,
                                    metadata = metadata)

      oracleAnnouncement = OracleAnnouncementV1TLV(announcementSignature =
                                                     announcementSignature,
                                                   metadata = metadata,
                                                   eventTLV = eventTLV)

      rValueA = rValueDAO.createAllAction(rValueDbs)
      actions = rValueA.flatMap(_ =>
        createAnnouncementAction(oracleAnnouncement))
      announcementWithId <- safeDatabase.run(actions)
    } yield {
      announcementWithId
    }
  }

  /** Gets announcements by event name, which are not guaranteed to be unique */
  def getByEventName(
      eventName: String): Future[Vector[OracleAnnouncementWithId]] = {
    val announcementsF = oracleAnnouncementDAO.findByEventName(eventName)
    val withId = announcementsF.flatMap { anns =>
      Future.traverse(anns)(a => getAnnouncement(a.id.get))
    }
    withId.map(_.flatten)
  }

  def getAttestments(id: Long): Future[Option[SchnorrAttestationTLV]] = {
    val announcementOptF = getAnnouncement(id)
    announcementOptF.flatMap {
      case Some(announcementWithId) =>
        announcementWithId.announcement match {
          case v1: OracleAnnouncementV1TLV =>
            val nonces: Vector[SchnorrNonce] =
              v1.noncesFlattened
            val nonceSignaturesF = oracleSchnorrNonceDAO.findByNonces(nonces)
            nonceSignaturesF.map { nonceSignatures =>
              SchnorrAttestation.fromAnnouncementAndNonceSignatures(
                v1,
                nonceSignatures)
            }
          case _: OracleAnnouncementV0TLV =>
            logger.warn(s"Getting attestments not implement yet for v0")
            Future.successful(None)
        }
      case None =>
        Future.successful(None)
    }
  }

  def updateNonceSignatureDbAction(
      nonceSignatureDb: NonceSignaturePairDb): DBIOAction[
    NonceSignaturePairDb,
    NoStream,
    Effect.Write] = {
    oracleSchnorrNonceDAO
      .updateOutcomeAttestation(nonceSignatureDb)
      .map(_ => nonceSignatureDb)
  }
}
