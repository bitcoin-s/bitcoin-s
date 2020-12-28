package org.bitcoins.dlc.oracle

import java.time.Instant
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.storage.EventDb

/** Represents an event that the oracle has committed to
  * Contains all the necessary information to construct
  * all the oracle TLV messages
  */
sealed trait OracleEvent {

  /** The nonces the oracle is committing to for this event */
  def nonces: Vector[SchnorrNonce]

  /** The oracle's public key */
  def pubkey: SchnorrPublicKey

  /** The name given to this event, may be a URI */
  def eventName: String

  /** The version of signing for this event */
  def signingVersion: SigningVersion

  /** The earliest expected time an outcome will be signed */
  def maturationTime: Instant

  /** A signature by the oracle of the hash of nonce and event name */
  def announcementSignature: SchnorrDigitalSignature

  def eventDescriptorTLV: EventDescriptorTLV

  def eventTLV: OracleEventTLV =
    OracleEventV0TLV(nonces,
                     UInt32(maturationTime.getEpochSecond),
                     eventDescriptorTLV,
                     eventName)

  def announcementTLV: OracleAnnouncementTLV = {
    eventTLV match {
      case v0TLV: OracleEventV0TLV =>
        OracleAnnouncementV0TLV(announcementSignature, pubkey, v0TLV)
    }
  }
}

/** An oracle event that has not been signed yet */
sealed trait PendingOracleEvent extends OracleEvent

/** An oracle event that has been signed */
sealed trait CompletedOracleEvent extends OracleEvent {
  def attestations: Vector[FieldElement]

  require(attestations.size == nonces.size,
          "Must have a signature for every nonce")

  def signatures: Vector[SchnorrDigitalSignature] =
    nonces
      .zip(attestations)
      .map(sigPieces => SchnorrDigitalSignature(sigPieces._1, sigPieces._2))
}

sealed trait EnumV0OracleEvent extends OracleEvent {
  override def eventDescriptorTLV: EnumEventDescriptorV0TLV
  def nonce: SchnorrNonce

  final override def nonces: Vector[SchnorrNonce] = Vector(nonce)
}

case class PendingEnumV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: EnumEventDescriptorV0TLV)
    extends PendingOracleEvent
    with EnumV0OracleEvent

case class CompletedEnumV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: EnumEventDescriptorV0TLV,
    attestation: FieldElement)
    extends CompletedOracleEvent
    with EnumV0OracleEvent {
  override def attestations: Vector[FieldElement] = Vector(attestation)
}

sealed trait RangeV0OracleEvent extends OracleEvent {
  override def eventDescriptorTLV: RangeEventDescriptorV0TLV
  def nonce: SchnorrNonce

  final override def nonces: Vector[SchnorrNonce] = Vector(nonce)
}

case class PendingRangeV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: RangeEventDescriptorV0TLV)
    extends PendingOracleEvent
    with RangeV0OracleEvent

case class CompletedRangeV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: RangeEventDescriptorV0TLV,
    attestation: FieldElement)
    extends CompletedOracleEvent
    with RangeV0OracleEvent {
  override def attestations: Vector[FieldElement] = Vector(attestation)

}

sealed trait DigitDecompositionV0OracleEvent extends OracleEvent {
  override def eventDescriptorTLV: DigitDecompositionEventDescriptorV0TLV
}

case class PendingDigitDecompositionV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonces: Vector[SchnorrNonce],
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: DigitDecompositionEventDescriptorV0TLV)
    extends PendingOracleEvent
    with DigitDecompositionV0OracleEvent

case class CompletedDigitDecompositionV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonces: Vector[SchnorrNonce],
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: DigitDecompositionEventDescriptorV0TLV,
    attestations: Vector[FieldElement])
    extends CompletedOracleEvent
    with DigitDecompositionV0OracleEvent

object OracleEvent {

  def fromEventDbs(eventDbs: Vector[EventDb]): OracleEvent = {
    val eventDb = eventDbs.head
    require(eventDbs.forall(_.eventDescriptorTLV == eventDb.eventDescriptorTLV),
            "EventDbs must all refer to the same event")

    (eventDb.eventDescriptorTLV, eventDb.attestationOpt) match {
      case (enum: EnumEventDescriptorV0TLV, Some(sig)) =>
        require(eventDbs.size == 1, "Enum events may only have one eventDb")
        CompletedEnumV0OracleEvent(eventDb.pubkey,
                                   eventDb.nonce,
                                   eventDb.eventName,
                                   eventDb.signingVersion,
                                   eventDb.maturationTime,
                                   eventDb.announcementSignature,
                                   enum,
                                   sig)
      case (enum: EnumEventDescriptorV0TLV, None) =>
        require(eventDbs.size == 1, "Enum events may only have one eventDb")
        PendingEnumV0OracleEvent(eventDb.pubkey,
                                 eventDb.nonce,
                                 eventDb.eventName,
                                 eventDb.signingVersion,
                                 eventDb.maturationTime,
                                 eventDb.announcementSignature,
                                 enum)
      case (range: RangeEventDescriptorV0TLV, Some(sig)) =>
        require(eventDbs.size == 1, "Range events may only have one eventDb")
        CompletedRangeV0OracleEvent(eventDb.pubkey,
                                    eventDb.nonce,
                                    eventDb.eventName,
                                    eventDb.signingVersion,
                                    eventDb.maturationTime,
                                    eventDb.announcementSignature,
                                    range,
                                    sig)
      case (range: RangeEventDescriptorV0TLV, None) =>
        require(eventDbs.size == 1, "Range events may only have one eventDb")
        PendingRangeV0OracleEvent(eventDb.pubkey,
                                  eventDb.nonce,
                                  eventDb.eventName,
                                  eventDb.signingVersion,
                                  eventDb.maturationTime,
                                  eventDb.announcementSignature,
                                  range)
      case (decomp: DigitDecompositionEventDescriptorV0TLV, Some(_)) =>
        require(eventDbs.forall(_.attestationOpt.isDefined),
                "Cannot have a partially signed event")
        val sortedEventDbs = eventDbs.sortBy(_.nonceIndex)

        val attestations = sortedEventDbs.flatMap(_.attestationOpt)

        CompletedDigitDecompositionV0OracleEvent(
          eventDb.pubkey,
          sortedEventDbs.map(_.nonce),
          eventDb.eventName,
          eventDb.signingVersion,
          eventDb.maturationTime,
          eventDb.announcementSignature,
          decomp,
          attestations
        )
      case (decomp: DigitDecompositionEventDescriptorV0TLV, None) =>
        require(eventDbs.forall(_.attestationOpt.isEmpty),
                "Cannot have a partially signed event")

        PendingDigitDecompositionV0OracleEvent(
          eventDb.pubkey,
          eventDbs.map(_.nonce),
          eventDb.eventName,
          eventDb.signingVersion,
          eventDb.maturationTime,
          eventDb.announcementSignature,
          decomp
        )
    }
  }

  /**
    * Verifies if the given attestations sign the outcomes of the given oracle announcement.
    */
  def verifyAttestations(
      announcement: OracleAnnouncementTLV,
      attestations: Vector[SchnorrDigitalSignature],
      signingVersion: SigningVersion): Boolean = {
    val nonces = announcement.eventTLV.nonces
    if (nonces.size != attestations.size || nonces != attestations.map(_.rx)) {
      false
    } else {
      announcement.eventTLV.eventDescriptor match {
        case enum: EnumEventDescriptorV0TLV =>
          require(attestations.size == 1)

          val sig = attestations.head
          enum.outcomes.exists { outcome =>
            val attestationType = EnumAttestation(outcome)
            val hash =
              signingVersion.calcOutcomeHash(enum, attestationType.bytes)
            announcement.publicKey.verify(hash, sig)
          }

        case dd: DigitDecompositionEventDescriptorV0TLV =>
          require(attestations.nonEmpty)

          val (validSign, attestationsToVerify) =
            dd match {
              case _: SignedDigitDecompositionEventDescriptor =>
                val signOutcomes = Vector(
                  DigitDecompositionSignAttestation(true),
                  DigitDecompositionSignAttestation(false))

                val validSign = signOutcomes.exists { attestationType =>
                  val hash =
                    signingVersion.calcOutcomeHash(dd, attestationType.bytes)
                  announcement.publicKey.verify(hash, attestations.head)
                }

                (validSign, attestations.tail)
              case _: UnsignedDigitDecompositionEventDescriptor =>
                (true, attestations)
            }

          lazy val digitOutcomes =
            0.until(dd.base.toInt)
              .map(DigitDecompositionAttestation.apply)

          lazy val validDigits = attestationsToVerify.forall { sig =>
            digitOutcomes.exists { attestationType =>
              val hash =
                signingVersion.calcOutcomeHash(dd, attestationType.bytes)
              announcement.publicKey.verify(hash, sig)
            }
          }

          validSign && validDigits

        case _: RangeEventDescriptorV0TLV => false
      }
    }
  }
}
