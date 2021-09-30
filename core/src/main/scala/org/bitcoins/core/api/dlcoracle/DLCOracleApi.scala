package org.bitcoins.core.api.dlcoracle

import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import java.nio.file.Path
import java.time.Instant
import scala.concurrent.Future

trait DLCOracleApi {
  def publicKey(): SchnorrPublicKey

  def stakingAddress(network: BitcoinNetwork): Bech32Address

  def listEventDbs(): Future[Vector[EventDb]]

  def listPendingEventDbs(): Future[Vector[EventDb]]

  def listEvents(): Future[Vector[OracleEvent]]

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

  @deprecated("Call createNewAnnouncement")
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

  @deprecated("Call signEnumAnnouncement")
  def signEnumEvent(
      eventName: String,
      outcome: EnumAttestation): Future[EventDb] = {
    signEnumAnnouncement(eventName, outcome)
  }

  def signEnumAnnouncement(
      eventName: String,
      outcome: EnumAttestation): Future[EventDb]

  @deprecated("Call signEnumAnnouncement")
  def signEnumEvent(
      oracleEventTLV: OracleEventTLV,
      outcome: EnumAttestation): Future[EventDb] = {
    signEnumAnnouncement(oracleEventTLV, outcome)
  }

  def signEnumAnnouncement(
      oracleEventTLV: OracleEventTLV,
      outcome: EnumAttestation): Future[EventDb]

  @deprecated("Call signAnnouncement")
  def signEvent(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[EventDb] = {
    signAnnouncement(nonce, outcome)
  }

  def signAnnouncement(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[EventDb]

  def signDigits(eventName: String, num: Long): Future[OracleEvent]

  def signDigits(oracleEventTLV: OracleEventTLV, num: Long): Future[OracleEvent]

  /** Deletes attestations for the given event
    *
    * WARNING: if previous signatures have been made public
    * the oracle private key will be revealed.
    */
  def deleteAttestations(eventName: String): Future[OracleEvent]

  /** Deletes attestations for the given event
    *
    * WARNING: if previous signatures have been made public
    * the oracle private key will be revealed.
    */
  def deleteAttestations(oracleEventTLV: OracleEventTLV): Future[OracleEvent]

  /** Signs the SHA256 hash of the given string using the oracle's signing key */
  def signMessage(message: String): SchnorrDigitalSignature = {
    signMessage(CryptoUtil.serializeForHash(message))
  }

  /** Signs the SHA256 hash of the given bytes using the oracle's signing key */
  def signMessage(message: ByteVector): SchnorrDigitalSignature

  /** Backup oracle database. Works only for the SQLite database driver.
    * @param location backup file location
    */
  def backup(location: Path): Future[Unit]
}
