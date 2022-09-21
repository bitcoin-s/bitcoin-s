package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.api.dlcoracle.OracleEvent
import org.bitcoins.core.api.dlcoracle.db.RValueDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.dlc.commons.oracle.{
  OracleCommonDataManagement,
  OracleMetadataDAO,
  OracleSchnorrNonceDAO
}
import org.bitcoins.dlc.oracle.util.EventDbUtil
import slick.dbio.DBIO

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class OracleDataManagement(daos: DLCOracleDAOs)(implicit
    ec: ExecutionContext)
    extends OracleCommonDataManagement {

  override protected val oracleMetadataDAO: OracleMetadataDAO =
    daos.oracleMetadataDAO

  override protected val oracleSchnorrNonceDAO: OracleSchnorrNonceDAO =
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

}
