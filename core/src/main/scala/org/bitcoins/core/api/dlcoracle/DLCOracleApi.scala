package org.bitcoins.core.api.dlcoracle

import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.Bech32Address
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

  def stakingAddress(network: BitcoinNetwork): Bech32Address

  def listEventDbs(): Future[Vector[EventDb]]

  def listPendingEventDbs(): Future[Vector[EventDb]]

  def listCompletedEventDbs(): Future[Vector[EventDb]]

  def listEvents(): Future[Vector[OracleEvent]]

  def listPendingEvents(): Future[Vector[OracleEvent]]

  def listCompletedEvents(): Future[Vector[OracleEvent]]

  def findEvent(oracleEventTLV: OracleEventTLV): Future[Option[OracleEvent]]

  def findEvent(eventName: String): Future[Option[OracleEvent]]

  @deprecated("Call createNewDigitDecompAnnouncement", "2021-09-30")
  def createNewDigitDecompEvent(
      eventName: String,
      maturationTime: Instant,
      base: UInt16,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): Future[OracleAnnouncementTLV] = {
    createNewDigitDecompAnnouncement(eventName = eventName,
                                     maturationTime = maturationTime,
                                     base = base,
                                     isSigned = isSigned,
                                     numDigits = numDigits,
                                     unit = unit,
                                     precision = precision)
  }

  def createNewDigitDecompAnnouncement(
      eventName: String,
      maturationTime: Instant,
      base: UInt16,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): Future[OracleAnnouncementTLV]

  @deprecated("Call createNewEnumAnnouncement", "2021-09-30")
  def createNewEnumEvent(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[OracleAnnouncementTLV] = {
    createNewEnumAnnouncement(eventName, maturationTime, outcomes)
  }

  def createNewEnumAnnouncement(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[OracleAnnouncementTLV]

  @deprecated("Call createNewAnnouncement", "2021-09-30")
  def createNewEvent(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorTLV,
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    OracleAnnouncementTLV] = {
    createNewAnnouncement(eventName, maturationTime, descriptor, signingVersion)
  }

  def createNewAnnouncement(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorTLV,
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    OracleAnnouncementTLV]

  @deprecated("Call signEnum", "2021-09-30")
  def signEnumEvent(
      eventName: String,
      outcome: EnumAttestation): Future[EventDb] = {
    signEnum(eventName, outcome)
  }

  /** Signs an enumerated announcement
    * @param eventName the event name of the announcement
    * @param outcome the outcome for the give announcement
    */
  def signEnum(eventName: String, outcome: EnumAttestation): Future[EventDb]

  @deprecated("Call signEnum", "2021-09-30")
  def signEnumEvent(
      oracleEventTLV: OracleEventTLV,
      outcome: EnumAttestation): Future[EventDb] = {
    signEnum(oracleEventTLV, outcome)
  }

  /** Signs an enumerated announcement
    * @param oracleEventTLV the tlv of the oracle event
    * @param outcome the outcome for the give announcement
    */
  def signEnum(
      oracleEventTLV: OracleEventTLV,
      outcome: EnumAttestation): Future[EventDb]

  @deprecated("Call createAttestation", "2021-09-30")
  def signEvent(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[EventDb] = {
    createAttestation(nonce, outcome)
  }

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
