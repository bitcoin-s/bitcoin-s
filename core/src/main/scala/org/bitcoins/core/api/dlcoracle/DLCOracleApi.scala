package org.bitcoins.core.api.dlcoracle

import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.{SchnorrNonce, SchnorrPublicKey}

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

  def createNewDigitDecompEvent(
      eventName: String,
      maturationTime: Instant,
      base: UInt16,
      isSigned: Boolean,
      numDigits: Int,
      unit: String,
      precision: Int32): Future[OracleAnnouncementTLV]

  def createNewRangedEvent(
      eventName: String,
      maturationTime: Instant,
      start: Int,
      count: Int,
      step: Int,
      unit: String,
      precision: Int): Future[OracleAnnouncementTLV]

  def createNewRangedEvent(
      eventName: String,
      maturationTime: Instant,
      start: Int32,
      count: UInt32,
      step: UInt16,
      unit: String,
      precision: Int32): Future[OracleAnnouncementTLV]

  def createNewEnumEvent(
      eventName: String,
      maturationTime: Instant,
      outcomes: Vector[String]): Future[OracleAnnouncementTLV]

  def createNewEvent(
      eventName: String,
      maturationTime: Instant,
      descriptor: EventDescriptorTLV,
      signingVersion: SigningVersion = SigningVersion.latest): Future[
    OracleAnnouncementTLV]

  def signEnumEvent(
      eventName: String,
      outcome: EnumAttestation): Future[EventDb]

  def signEnumEvent(
      oracleEventTLV: OracleEventTLV,
      outcome: EnumAttestation): Future[EventDb]

  def signEvent(
      nonce: SchnorrNonce,
      outcome: DLCAttestationType): Future[EventDb]

  def signDigits(eventName: String, num: Long): Future[OracleEvent]

  def signDigits(oracleEventTLV: OracleEventTLV, num: Long): Future[OracleEvent]
}
