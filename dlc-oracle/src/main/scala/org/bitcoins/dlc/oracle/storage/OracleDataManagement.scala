package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.api.dlcoracle.OracleEvent
import org.bitcoins.core.api.dlcoracle.db.RValueDb
import org.bitcoins.core.dlc.oracle
import org.bitcoins.core.dlc.oracle.{
  NonceSignaturePairDb,
  OracleMetadataDb,
  OracleMetadataDbHelper
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv.{
  BaseOracleAnnouncement,
  EventDescriptorDLCType,
  FixedOracleEventTimestamp,
  OracleAnnouncementV1TLV,
  OracleEventV1TLV,
  OracleMetadata
}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.{ECPrivateKey, SchnorrNonce, SchnorrPublicKey}
import org.bitcoins.dlc.oracle.util.EventDbUtil
import slick.dbio.{DBIO, DBIOAction, Effect, NoStream}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class OracleDataManagement(daos: DLCOracleDAOs)(implicit
    ec: ExecutionContext) {
  private val oracleMetadataDAO: OracleMetadataDAO = daos.oracleMetadataDAO

  private val oracleSchnorrNonceDAO: OracleSchnorrNonceDAO =
    daos.oracleSchnorrNonceDAO

  private val eventDAO: EventDAO = daos.eventDAO
  private val eventOutcomeDAO: EventOutcomeDAO = daos.outcomeDAO
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
    BaseOracleAnnouncement] = {
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

      eventOutcomeDbs = EventDbUtil.toEventOutcomeDbs(
        oracleAnnouncementV0TLV = oracleAnnouncement,
        signingVersion = signingVersion)

      eventDbs = EventDbUtil.toEventDbs(baseOracleAnnouncement =
                                          oracleAnnouncement,
                                        eventName = eventName,
                                        signingVersion = signingVersion)

      rValueA = rValueDAO.createAllAction(rValueDbs)
      eventDbsA = eventDAO.createAllAction(eventDbs)
      eventOutcomeDbsA = eventOutcomeDAO.createAllAction(eventOutcomeDbs)
      metadataAction = createOracleMetaDataAction(metadata)
      actions = DBIO.seq(rValueA, eventDbsA, eventOutcomeDbsA, metadataAction)
      _ <- safeDatabase.run(actions)
    } yield {
      OracleEvent.fromEventDbs(eventDbs, Some(metadata)).announcementTLV
    }
  }

  def getOracleMetadata(id: Long): Future[Option[OracleMetadata]] = {
    val action = for {
      metadataDbOpt <- oracleMetadataDAO.findByPrimaryKeyAction(id)
      nonceSignaturesDbs <- oracleSchnorrNonceDAO.findByIdAction(id)
    } yield {
      metadataDbOpt.map { metadataDb =>
        OracleMetadata.fromDbs(metadataDb, nonceSignaturesDbs)
      }
    }

    safeDatabase.run(action)
  }

  def createOracleMetaDataAction(oracleMetaData: OracleMetadata): DBIOAction[
    (OracleMetadataDb, Vector[NonceSignaturePairDb]),
    NoStream,
    Effect.Write] = {
    val metdataDb = OracleMetadataDbHelper.fromOracleMetadata(oracleMetaData)

    val createMetaDataA = oracleMetadataDAO.createAction(metdataDb)

    val combinedA = for {
      metadataDbWithId <- createMetaDataA
      id = metadataDbWithId.id.get
      nonceSigPairs = oracleMetaData.nonceSignatures
      withIds = nonceSigPairs.map(p =>
        oracle.NonceSignaturePairDb(id, p.nonce, p.nonceSignature))
      nonceSignatureDbs <- oracleSchnorrNonceDAO.createAllAction(withIds)
    } yield (metadataDbWithId, nonceSignatureDbs)

    combinedA
  }

  def createOracleMetadata(oracleMetaData: OracleMetadata): Future[
    (OracleMetadataDb, Vector[NonceSignaturePairDb])] = {
    val action = createOracleMetaDataAction(oracleMetaData)
    safeDatabase.run(action)
  }

  def findMetadataByAttestationPubKeyAction(
      attestationPubKey: SchnorrPublicKey): DBIOAction[
    Option[OracleMetadata],
    NoStream,
    Effect.Read] = {
    val metadataOptA =
      oracleMetadataDAO.findByAttestationPubKeyAction(attestationPubKey)
    for {
      metadataOpt <- metadataOptA
      metdataOpt <- {
        metadataOpt match {
          case Some(metadata) =>
            oracleSchnorrNonceDAO
              .findByIdAction(metadata.id.get)
              .map(nonces => OracleMetadata.fromDbs(metadata, nonces))
              .map(Some(_))
          case None => DBIO.successful(None)
        }
      }
    } yield metdataOpt
  }

  def findMetadataByAttestationPubKey(
      attestationPubKey: SchnorrPublicKey): Future[Option[OracleMetadata]] = {
    val action = findMetadataByAttestationPubKeyAction(attestationPubKey)
    safeDatabase.run(action)
  }

  def findMetadataByNonce(
      nonce: SchnorrNonce): Future[Option[OracleMetadata]] = {

    for {
      nonceOpt <- oracleSchnorrNonceDAO.findByNonce(nonce)
      metadataOpt <- {
        nonceOpt match {
          case Some(db) => getOracleMetadata(db.id)
          case None     => Future.successful(None)
        }
      }
    } yield metadataOpt

  }

}
