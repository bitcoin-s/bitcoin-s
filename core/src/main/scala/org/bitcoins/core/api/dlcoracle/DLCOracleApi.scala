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
  def announcementPublicKey(): SchnorrPublicKey

  def attestationPublicKey(): SchnorrPublicKey

  def oracleName(): Future[Option[String]]

  def setOracleName(name: String): Future[Unit]

  def stakingAddress(network: BitcoinNetwork): Bech32Address

  def listEventDbs(): Future[Vector[EventDb]]

  def listPendingEventDbs(): Future[Vector[EventDb]]

  def listCompletedEventDbs(): Future[Vector[EventDb]]

  def listEvents(): Future[Vector[OracleEvent]]

  def listPendingEvents(): Future[Vector[PendingOracleEvent]]

  def listCompletedEvents(): Future[Vector[CompletedOracleEvent]]

  def findEvent(
      announcement: BaseOracleAnnouncement): Future[Option[OracleEvent]]

  def findEvent(eventName: String): Future[Option[OracleEvent]]

  def createNewDigitDecompAnnouncement(
      eventName: String,
      maturationTime: Instant,
      base: UInt8,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): Future[BaseOracleAnnouncement]

  def createNewEnumAnnouncement(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[BaseOracleAnnouncement]

  def createNewAnnouncement(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorDLCType,
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    BaseOracleAnnouncement]

  /** Signs an enumerated announcement
    * @param eventName the event name of the announcement
    * @param outcome the outcome for the give announcement
    */
  def signEnum(
      eventName: String,
      outcome: EnumAttestation): Future[CompletedEnumV0OracleEvent]

  /** Signs an enumerated announcement
    * @param oracleEventTLV the tlv of the oracle event
    * @param outcome the outcome for the give announcement
    */
  def signEnum(
      oracleEventTLV: BaseOracleEvent,
      outcome: EnumAttestation): Future[CompletedEnumV0OracleEvent]

  def createAttestation(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[EventDb]

  def signDigits(
      eventName: String,
      num: Long): Future[CompletedDigitDecompositionV0OracleEvent]

  def signDigits(
      announcement: BaseOracleAnnouncement,
      num: Long): Future[CompletedDigitDecompositionV0OracleEvent]

  /** Deletes an announcement with the given name
    * WARNING: If this announcement has been published widely
    * users will not be able to settle their DLCs.
    * You likely should only use this in testing scenarios
    * @return the deleted announcement
    */
  def deleteAnnouncement(eventName: String): Future[BaseOracleAnnouncement]

  /** Deletes an announcement with the given name
    * WARNING: If this announcement has been published widely
    * users will not be able to settle their DLCs.
    * You likely should only use this in testing scenarios
    * @return the deleted announcement
    */
  def deleteAnnouncement(
      announcementTLV: BaseOracleAnnouncement): Future[BaseOracleAnnouncement]

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
  def deleteAttestation(
      announcement: BaseOracleAnnouncement): Future[OracleEvent]

  /** Signs the SHA256 hash of the given string using the oracle's signing key */
  def signMessage(message: String): SchnorrDigitalSignature = {
    signMessage(CryptoUtil.serializeForHash(message))
  }

  /** Signs the SHA256 hash of the given bytes using the oracle's signing key */
  def signMessage(message: ByteVector): SchnorrDigitalSignature

  /** Returns the staking address private key in wallet import format
    * so a user can take it an recover the funds in another wallet
    */
  def exportSigningKeyWIF: String
}
