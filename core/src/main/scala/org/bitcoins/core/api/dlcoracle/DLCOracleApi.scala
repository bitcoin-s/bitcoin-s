package org.bitcoins.core.api.dlcoracle

import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import java.time.Instant
import scala.concurrent.Future

trait DLCOracleApi {
  def publicKey(): SchnorrPublicKey

  def oracleName(): Future[Option[String]]

  def setOracleName(name: String): Future[Unit]

  def listEventDbs(): Future[Vector[EventDb]]

  def listPendingEventDbs(): Future[Vector[EventDb]]

  def listCompletedEventDbs(): Future[Vector[EventDb]]

  def listEvents(): Future[Vector[OracleEvent]]

  def listPendingEvents(): Future[Vector[OracleEvent]]

  def listCompletedEvents(): Future[Vector[OracleEvent]]

  def findEvent(oracleEventTLV: OracleEventTLV): Future[Option[OracleEvent]]

  def findEvent(eventName: String): Future[Option[OracleEvent]]

  def createNewDigitDecompAnnouncement(
      eventName: String,
      maturationTime: Instant,
      base: UInt16,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): Future[OracleAnnouncementTLV]

  def createNewEnumAnnouncement(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[OracleAnnouncementTLV]

  def createNewAnnouncement(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorTLV,
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    OracleAnnouncementTLV]

  /** Signs an enumerated announcement
    * @param eventName the event name of the announcement
    * @param outcome the outcome for the give announcement
    */
  def signEnum(eventName: String, outcome: EnumAttestation): Future[EventDb]

  /** Signs an enumerated announcement
    * @param oracleEventTLV the tlv of the oracle event
    * @param outcome the outcome for the give announcement
    */
  def signEnum(
      oracleEventTLV: OracleEventTLV,
      outcome: EnumAttestation): Future[EventDb]

  def createAttestation(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[EventDb]

  def signDigits(eventName: String, num: Long): Future[OracleEvent]

  def signDigits(oracleEventTLV: OracleEventTLV, num: Long): Future[OracleEvent]

  /** Deletes an announcement with the given name
    * WARNING: If this announcement has been published widely
    * users will not be able to settle their DLCs.
    * You likely should only use this in testing scenarios
    * @return the deleted announcement
    */
  def deleteAnnouncement(eventName: String): Future[OracleAnnouncementTLV]

  /** Deletes an announcement with the given name
    * WARNING: If this announcement has been published widely
    * users will not be able to settle their DLCs.
    * You likely should only use this in testing scenarios
    * @return the deleted announcement
    */
  def deleteAnnouncement(
      announcementTLV: OracleAnnouncementTLV): Future[OracleAnnouncementTLV]

  /** Deletes attestations for the given event
    *
    * WARNING: if previous signatures have been made public
    * the oracle private key will be revealed.
    */
  def deleteAttestation(eventName: String): Future[OracleEvent]

  /** Deletes attestations for the given event
    *
    * WARNING: if previous signatures have been made public
    * the oracle private key will be revealed.
    */
  def deleteAttestation(oracleEventTLV: OracleEventTLV): Future[OracleEvent]

  /** Signs the SHA256 hash of the given string using the oracle's signing key */
  def signMessage(message: String): SchnorrDigitalSignature = {
    signMessage(CryptoUtil.serializeForHash(message))
  }

  /** Signs the SHA256 hash of the given bytes using the oracle's signing key */
  def signMessage(message: ByteVector): SchnorrDigitalSignature
}
