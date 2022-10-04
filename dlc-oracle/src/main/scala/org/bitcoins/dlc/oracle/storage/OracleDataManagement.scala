package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.api.dlcoracle.db.RValueDb
import org.bitcoins.core.dlc.oracle.{
  AnnouncementAttestationPair,
  NonceSignaturePairDb,
  OracleAnnouncementWithId
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.crypto.{ECPrivateKey, SchnorrNonce}
import org.bitcoins.dlc.commons.oracle.{
  EventOutcomeDAO,
  OracleAnnouncementDataDAO,
  OracleCommonDataManagement,
  OracleMetadataDAO,
  OracleSchnorrNonceDAO
}
import slick.dbio.{DBIO, DBIOAction, Effect, NoStream}

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

  override protected val eventOutcomeDAO: EventOutcomeDAO = daos.outcomeDAO

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
      dbs <- oracleAnnouncementDAO.findByEventName(eventName)
      _ = require(dbs.isEmpty, s"Event name ($eventName) is already being used")

      epoch = UInt32(maturationTime.getEpochSecond)

      eventTLV = OracleEventV1TLV(eventDescriptor = descriptor,
                                  eventName,
                                  FixedOracleEventTimestamp(epoch))

      announcementSignature = OracleAnnouncementV1TLV
        .buildAnnouncementSignature(announcementPrivKey = announcementPrivKey,
                                    signingVersion = signingVersion,
                                    eventTLV = eventTLV)

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

  def deleteAnnouncementAction(
      announcement: OracleAnnouncementV1TLV): DBIOAction[
    OracleAnnouncementV1TLV,
    NoStream,
    Effect.Read with Effect.Write] = {
    val announcementWithIdAction = getByEventNameAction(
      announcement.eventTLV.eventId)
    for {
      annsWithId <- announcementWithIdAction
      deleteActionNested = annsWithId.map { annWithId =>
        val nonceDeleteA =
          oracleSchnorrNonceDAO.deleteByAnnouncementId(annWithId.id)
        val metadataA =
          oracleMetadataDAO.deleteByAnnouncementIdAction(annWithId.id)
        val announcementDataA =
          oracleAnnouncementDAO.deleteByAnnouncementIdAction(annWithId.id)
        for {
          _ <- nonceDeleteA
          _ <- metadataA
          _ <- announcementDataA
        } yield ()
      }
      _ <- DBIO.sequence(deleteActionNested)
    } yield announcement
  }

  def deleteAnnouncement(announcement: OracleAnnouncementV1TLV): Future[
    OracleAnnouncementV1TLV] = {
    val action = deleteAnnouncementAction(announcement)
    safeDatabase.run(action)
  }

  def getByEventNameAction(eventName: String): DBIOAction[
    Vector[OracleAnnouncementWithId],
    NoStream,
    Effect.Read] = {
    val announcementsF = oracleAnnouncementDAO.findByEventNameAction(eventName)
    val withId = announcementsF.flatMap { anns =>
      val nested = anns.map(a => getAnnouncementAction(a.id.get))
      DBIOAction
        .sequence(nested)
        .map(_.flatten)
    }
    withId
  }

  /** Gets announcements by event name, which are not guaranteed to be unique */
  def getByEventName(
      eventName: String): Future[Vector[OracleAnnouncementWithId]] = {
    val action = getByEventNameAction(eventName)
    safeDatabase.run(action)
  }

  def getAttestmentsAction(id: Long): DBIOAction[
    Option[SchnorrAttestationTLV],
    NoStream,
    Effect.Read] = {
    val announcementOptA = getAnnouncementAction(id)
    announcementOptA.flatMap {
      case Some(announcementWithId) =>
        announcementWithId.announcement match {
          case v1: OracleAnnouncementV1TLV =>
            val nonces: Vector[SchnorrNonce] =
              v1.noncesFlattened
            val nonceSignaturesF =
              oracleSchnorrNonceDAO.findByNoncesAction(nonces)
            nonceSignaturesF.map { nonceSignatures =>
              //nonces may be out of order for the databases
              val sortedNonceSignatures = sortNonces(
                nonceSignatureDbs = nonceSignatures,
                nonces = OrderedNonces(v1.nonces.head)
              )
              SchnorrAttestation.fromAnnouncementAndNonceSignatures(
                v1,
                sortedNonceSignatures)
            }
        }
      case None =>
        DBIO.successful(None)
    }
  }

  def getAttestments(id: Long): Future[Option[SchnorrAttestationTLV]] = {
    val action = getAttestmentsAction(id)
    safeDatabase.run(action)
  }

  /** Helper functions to sort nonces in the way the are ordered in the announcement */
  private def sortNonces(
      nonceSignatureDbs: Vector[NonceSignaturePairDb],
      nonces: OrderedNonces): Vector[NonceSignaturePairDb] = {
    val map: Map[SchnorrNonce, NonceSignaturePairDb] =
      nonceSignatureDbs.map(n => (n.nonce, n)).toMap

    val builder = Vector.newBuilder[NonceSignaturePairDb]
    nonces.toVector.foreach { n =>
      builder.+=(map(n))
    }
    builder.result()
  }

  def updateNonceSignatureDbAction(
      nonceSignatureDb: NonceSignaturePairDb): DBIOAction[
    NonceSignaturePairDb,
    NoStream,
    Effect.Write] = {
    oracleSchnorrNonceDAO
      .updateNonceSignatureDb(nonceSignatureDb)
      .map(_ => nonceSignatureDb)
  }

  def getNonceSchnorrDbAction(announcementId: Long): DBIOAction[
    Vector[NonceSignaturePairDb],
    NoStream,
    Effect.Read] = {
    oracleSchnorrNonceDAO.findByIdAction(announcementId)
  }

  /** Gets announcements that do not have attestations */
  def getPendingAnnouncementsAction: DBIOAction[
    Vector[OracleAnnouncementWithId],
    NoStream,
    Effect.Read] = {
    for {
      announcements <- oracleAnnouncementDAO.findAllAction()
      announcementsNoAttestation <- {
        val nested = announcements.map { a =>
          getAttestmentsAction(a.id.get).map {
            case Some(_) => None
            case None    => Some(a)
          }
        }
        DBIOAction
          .sequence(nested)
          .map(_.flatten)
      }
      fullAnnouncements <- {
        val nested =
          announcementsNoAttestation.map(a => getAnnouncementAction(a.id.get))
        DBIOAction
          .sequence(nested)
          .map(_.flatten)
      }
    } yield fullAnnouncements
  }

  def getPendingAnnouncements: Future[Vector[OracleAnnouncementWithId]] = {
    val action = getPendingAnnouncementsAction
    safeDatabase.run(action)
  }

  def getCompletedAnnouncementsAction: DBIOAction[
    Vector[AnnouncementAttestationPair],
    NoStream,
    Effect.Read] = {
    for {
      announcements <- oracleAnnouncementDAO.findAllAction()
      announcementAttestationPairs <- {
        val nested: Vector[DBIOAction[
          Option[AnnouncementAttestationPair],
          NoStream,
          Effect.Read]] = {
          announcements.map { a =>
            getAttestmentsAction(a.id.get).flatMap {
              case Some(attestation) =>
                getAnnouncementAction(a.id.get).map {
                  case Some(annWithId) =>
                    val p = AnnouncementAttestationPair(annWithId.id,
                                                        annWithId.announcement,
                                                        attestation)
                    Some(p)
                  case None =>
                    sys.error(
                      s"Must have announcement in database if we have id, got=${a.id}")
                }
              case None =>
                DBIO.successful(None)
            }
          }
        }
        DBIOAction
          .sequence(nested)
          .map(_.flatten)
      }
    } yield announcementAttestationPairs
  }

  /** Gets announcements that do have attestations */
  def getCompletedAnnouncements: Future[Vector[AnnouncementAttestationPair]] = {
    val action = getCompletedAnnouncementsAction
    safeDatabase.run(action)
  }
}
