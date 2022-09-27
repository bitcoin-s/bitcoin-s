package org.bitcoins.core.api.dlcoracle

import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.dlc.oracle.{NonceSignaturePairDb}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.core.util.sorted.{OrderedNonces, OrderedSchnorrSignatures}
import org.bitcoins.crypto._

import java.time.Instant

/** Represents an event that the oracle has committed to
  * Contains all the necessary information to construct
  * all the oracle TLV messages
  */
sealed trait OracleEvent {

  /** The nonces the oracle is committing to for this event */
  def nonces: OrderedNonces

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

  def eventDescriptorTLV: BaseEventDescriptor

  def metadataOpt: Option[OracleMetadata]

  def eventTLV: BaseOracleEvent = {
    val timestamps = FixedOracleEventTimestamp(
      UInt32(maturationTime.getEpochSecond))
    eventDescriptorTLV match {
      case e: EventDescriptorDLCType =>
        OracleEventV1TLV(eventDescriptor = e,
                         eventId = eventName,
                         timestamps = timestamps)
      case tlv: EventDescriptorTLV =>
        require(eventDbsOpt.isDefined,
          s"Event dbs must be defined to figure out ordering of nonces")
        val v0NonceOrder = eventDbsOpt.get.sortBy(_.nonceIndex).map(_.nonce)
        OracleEventV0TLV(v0NonceOrder,
                         UInt32(maturationTime.getEpochSecond),
                         tlv,
                         eventName)
    }
  }

  def announcementTLV: BaseOracleAnnouncement = {
    eventTLV match {
      case v0TLV: OracleEventV0TLV =>
        OracleAnnouncementV0TLV(announcementSignature, pubkey, v0TLV)
      case v1: OracleEventV1TLV =>
        require(metadataOpt.isDefined,
                s"Metadata must be defined to build an announcement v1")
        OracleAnnouncementV1TLV(announcementSignature = announcementSignature,
                                eventTLV = v1,
                                metadata = metadataOpt.get)
    }
  }

  /** These are needed for old announcements/attesatations that do not follow the requirement
    * to order nonces
    */
  protected def eventDbsOpt: Option[Vector[EventDb]]
}

/** An oracle event that has not been signed yet */
sealed trait PendingOracleEvent extends OracleEvent

/** An oracle event that has been signed */
sealed trait CompletedOracleEvent extends OracleEvent {
  def attestations: Vector[FieldElement]

  require(attestations.size == nonces.size,
          "Must have a signature for every nonce")

  def signatures: OrderedSchnorrSignatures = {
    val unsorted = nonces.toVector
      .zip(attestations)
      .map(sigPieces => SchnorrDigitalSignature(sigPieces._1, sigPieces._2))
    OrderedSchnorrSignatures.fromUnsorted(unsorted)
  }

  def oracleAttestmentV0TLV: OracleAttestmentTLV = {
    announcementTLV match {
      case ann: OracleAnnouncementV0TLV =>
        //v0 announcements do not have a invariant stating that nonces neeed to be sorted
        //a specific way, so we need to use the unsorted variant to make sure
        //announcementSignatures evaluate to true
        val unsorted = ann.eventTLV.nonces
          .zip(attestations)
          .map(sigPieces => SchnorrDigitalSignature(sigPieces._1, sigPieces._2))
        OracleAttestmentV0TLV(eventName,
                              pubkey,
          unsorted,
                              outcomes.map(_.outcomeString))
      case _: OracleAnnouncementV1TLV =>
        SchnorrAttestationTLV(eventId = eventName,
                              publicKey = pubkey,
                              sigs = signatures,
                              outcomes = outcomes.map(_.outcomeString))
    }
  }

  def outcomes: Vector[DLCAttestationType]

  def dlcOutcome: DLCOutcomeType
}

sealed trait EnumV0OracleEvent extends OracleEvent {
  override def eventDescriptorTLV: BaseEnumEventDescriptor
  def nonce: SchnorrNonce

  final override def nonces: OrderedNonces = OrderedNonces(nonce)
}

case class PendingEnumV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: BaseEnumEventDescriptor,
    metadataOpt: Option[OracleMetadata],
    eventDbsOpt: Option[Vector[EventDb]])
    extends PendingOracleEvent
    with EnumV0OracleEvent

case class CompletedEnumV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: BaseEnumEventDescriptor,
    metadataOpt: Option[OracleMetadata],
    outcome: EnumAttestation,
    attestation: FieldElement,
    eventDbsOpt: Option[Vector[EventDb]])
    extends CompletedOracleEvent
    with EnumV0OracleEvent {
  require(
    OracleEvent.verifyAttestations(announcementTLV,
                                   oracleAttestmentV0TLV,
                                   signingVersion),
    s"Signatures given are invalid, eventId=${announcementTLV.eventTLV.eventId}"
  )

  val signature: SchnorrDigitalSignature =
    SchnorrDigitalSignature(nonce, attestation)
  override def attestations: Vector[FieldElement] = Vector(attestation)

  override def outcomes: Vector[DLCAttestationType] = Vector(outcome)

  override def dlcOutcome: DLCOutcomeType = EnumOutcome(outcome.outcomeString)
}

sealed trait DigitDecompositionV0OracleEvent extends OracleEvent {
  override def eventDescriptorTLV: BaseNumericEventDescriptorTLV
}

case class PendingDigitDecompositionV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonces: OrderedNonces,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: BaseNumericEventDescriptorTLV,
    metadataOpt: Option[OracleMetadata],
    eventDbsOpt: Option[Vector[EventDb]])
    extends PendingOracleEvent
    with DigitDecompositionV0OracleEvent

case class CompletedDigitDecompositionV0OracleEvent(
    pubkey: SchnorrPublicKey,
    nonces: OrderedNonces,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: BaseNumericEventDescriptorTLV,
    metadataOpt: Option[OracleMetadata],
    dlcOutcome: NumericDLCOutcomeType,
    attestations: Vector[FieldElement],
    eventDbsOpt: Option[Vector[EventDb]])
    extends CompletedOracleEvent
    with DigitDecompositionV0OracleEvent {

  require(
    OracleEvent.verifyAttestations(announcementTLV,
                                   oracleAttestmentV0TLV,
                                   signingVersion),
    s"Signatures given are invalid for eventId=${announcementTLV.eventTLV.eventId}"
  )

  val outcomeBase10: Long = {
    val (digits, positive) = dlcOutcome match {
      case UnsignedNumericOutcome(digits) =>
        (digits, true)
      case SignedNumericOutcome(positive, digits) =>
        (digits, positive)
    }

    val base = eventDescriptorTLV.base.toInt
    val numDigits = eventDescriptorTLV.numDigits.toInt

    val num = NumberUtil.fromDigits(digits, base, numDigits)

    if (positive) num
    else num * -1
  }

  override def outcomes: Vector[DigitDecompositionAttestationType] =
    dlcOutcome match {
      case UnsignedNumericOutcome(digits) =>
        digits.map(DigitDecompositionAttestation)
      case SignedNumericOutcome(positive, digits) =>
        val sign = DigitDecompositionSignAttestation(positive)
        sign +: digits.map(DigitDecompositionAttestation)
    }
}

object OracleEvent {

  def fromAnnouncementAndNonceSignatureDb(
      announcement: OracleAnnouncementV1TLV,
      nonceSignatures: Vector[NonceSignaturePairDb]): OracleEvent = {
    require(nonceSignatures.forall(db =>
              announcement.nonces.toVector.contains(db.nonce)),
            s"Nonce signature dbs must be in the announcement")
    val schnorrAttestationOpt = SchnorrAttestation
      .fromAnnouncementAndNonceSignatures(announcement, nonceSignatures)
    fromAnnouncementAndAttestations(announcementV1 = announcement,
                                    attestationOpt = schnorrAttestationOpt)
  }

  def fromAnnouncement(
      announcementV1: OracleAnnouncementV1TLV): PendingOracleEvent = {
    announcementV1.eventTLV.eventDescriptor match {
      case enum: EnumEventDescriptorDLCSubType =>
        PendingEnumV0OracleEvent(
          announcementV1.attestationPublicKey,
          announcementV1.metadata.attestations.nonces.head,
          announcementV1.eventTLV.eventId,
          signingVersion = SigningVersion.latest,
          maturationTime = announcementV1.eventTLV.maturation,
          announcementSignature = announcementV1.announcementSignature,
          eventDescriptorTLV = enum,
          metadataOpt = Some(announcementV1.metadata)
        )
      case numeric: NumericEventDescriptorDLCType =>
        PendingDigitDecompositionV0OracleEvent(
          pubkey = announcementV1.attestationPublicKey,
          nonces = announcementV1.metadata.attestations.nonces,
          eventName = announcementV1.eventTLV.eventId,
          signingVersion = SigningVersion.latest,
          maturationTime = announcementV1.eventTLV.maturation,
          announcementSignature = announcementV1.announcementSignature,
          eventDescriptorTLV = numeric,
          metadataOpt = Some(announcementV1.metadata)
        )
    }
  }

  def fromAnnouncementAttestationPair(
      announcementV1: OracleAnnouncementV1TLV,
      attestation: SchnorrAttestationTLV): CompletedOracleEvent = {
    announcementV1.eventTLV.eventDescriptor match {
      case enum: EnumEventDescriptorDLCSubType =>
        CompletedEnumV0OracleEvent(
          pubkey = announcementV1.attestationPublicKey,
          nonce = announcementV1.metadata.attestations.nonces.head,
          eventName = announcementV1.eventTLV.eventId,
          signingVersion = SigningVersion.latest,
          maturationTime = announcementV1.eventTLV.maturation,
          announcementSignature = announcementV1.announcementSignature,
          eventDescriptorTLV = enum,
          metadataOpt = Some(announcementV1.metadata),
          outcome = EnumAttestation(attestation.outcomes.head),
          attestation = attestation.sigs.head.sig
        )
      case decomp: NumericEventDescriptorDLCType =>
        val numericOutcome = decomp match {
          case _: UnsignedDigitDecompositionEventDescriptor |
              _: UnsignedDigitDecompositionEventDescriptorDLCType =>
            val digits = attestation.outcomes.map(_.normStr.toInt)
            UnsignedNumericOutcome(digits)
          case _: SignedDigitDecompositionEventDescriptor |
              _: SignedDigitDecompositionEventDescriptorDLCType =>
            val positive = attestation.outcomes.head == "+"
            val digits = attestation.outcomes.tail.map(_.normStr.toInt)
            SignedNumericOutcome(positive, digits)

        }

        CompletedDigitDecompositionV0OracleEvent(
          pubkey = announcementV1.attestationPublicKey,
          nonces = announcementV1.metadata.attestations.nonces,
          eventName = announcementV1.eventTLV.eventId,
          signingVersion = SigningVersion.latest,
          maturationTime = announcementV1.eventTLV.maturation,
          announcementSignature = announcementV1.announcementSignature,
          eventDescriptorTLV = decomp,
          metadataOpt = Some(announcementV1.metadata),
          dlcOutcome = numericOutcome,
          attestations = attestation.sigs.map(_.sig).toVector
        )
    }
  }

  def fromAnnouncementAndAttestations(
      announcementV1: OracleAnnouncementV1TLV,
      attestationOpt: Option[SchnorrAttestationTLV]): OracleEvent = {
    attestationOpt match {
      case Some(attestation) =>
        fromAnnouncementAttestationPair(announcementV1, attestation)
      case None =>
        fromAnnouncement(announcementV1)
    }
  }

  def fromEventDbs(
      eventDbs: Vector[EventDb],
      metadataOpt: Option[OracleMetadata]): OracleEvent = {
    val eventDb = eventDbs.head
    require(eventDbs.forall(_.eventDescriptorTLV == eventDb.eventDescriptorTLV),
            "EventDbs must all refer to the same event")

    eventDb.attestationOpt match {
      case Some(_) => fromCompletedEventDbs(eventDbs, metadataOpt)
      case None    => fromPendingEventDbs(eventDbs, metadataOpt)
    }

  }

  def fromPendingEventDbs(
      eventDbs: Vector[EventDb],
      metadataOpt: Option[OracleMetadata]): PendingOracleEvent = {
    val eventDb = eventDbs.head
    require(eventDbs.forall(_.eventDescriptorTLV == eventDb.eventDescriptorTLV),
            "EventDbs must all refer to the same event")
    require(eventDbs.forall(_.attestationOpt.isEmpty),
            s"To make a pending event db we cannot have attestations")

    eventDb.eventDescriptorTLV match {
      case enum: BaseEnumEventDescriptor =>
        require(eventDbs.size == 1, "Enum events may only have one eventDb")
        PendingEnumV0OracleEvent(eventDb.pubkey,
                                 eventDb.nonce,
                                 eventDb.eventName,
                                 eventDb.signingVersion,
                                 eventDb.maturationTime,
                                 eventDb.announcementSignature,
                                 enum,
                                 metadataOpt,
          Some(eventDbs))

      case decomp: BaseNumericEventDescriptorTLV =>
        require(eventDbs.forall(_.attestationOpt.isEmpty),
                "Cannot have a partially signed event")

        val sortedEventDbs = eventDbs.sortBy(_.nonceIndex)

        PendingDigitDecompositionV0OracleEvent(
          eventDb.pubkey,
          OrderedNonces(sortedEventDbs.map(_.nonce)),
          eventDb.eventName,
          eventDb.signingVersion,
          eventDb.maturationTime,
          eventDb.announcementSignature,
          decomp,
          metadataOpt,
          Some(eventDbs)
        )
    }
  }

  def fromCompletedEventDbs(
      eventDbs: Vector[EventDb],
      metadataOpt: Option[OracleMetadata]): CompletedOracleEvent = {
    val eventDb = eventDbs.head
    require(eventDbs.forall(_.eventDescriptorTLV == eventDb.eventDescriptorTLV),
            "EventDbs must all refer to the same event")
    require(eventDbs.forall(_.attestationOpt.isDefined),
            s"To make a completed event db we must have attestations")

    eventDb.eventDescriptorTLV match {
      case enum: BaseEnumEventDescriptor =>
        require(eventDbs.size == 1, "Enum events may only have one eventDb")
        CompletedEnumV0OracleEvent(
          pubkey = eventDb.pubkey,
          nonce = eventDb.nonce,
          eventName = eventDb.eventName,
          signingVersion = eventDb.signingVersion,
          maturationTime = eventDb.maturationTime,
          announcementSignature = eventDb.announcementSignature,
          eventDescriptorTLV = enum,
          metadataOpt = metadataOpt,
          outcome = EnumAttestation(eventDb.outcomeOpt.get),
          attestation = eventDb.attestationOpt.get,
          Some(eventDbs)
        )

      case decomp: BaseNumericEventDescriptorTLV =>
        require(eventDbs.forall(_.attestationOpt.isDefined),
                "Cannot have a partially signed event")
        val sortedEventDbs = eventDbs.sortBy(_.nonceIndex)

        val attestations = sortedEventDbs.flatMap(_.attestationOpt)

        val dlcOutcome = decomp match {
          case _: SignedDigitDecompositionEventDescriptor |
              _: SignedDigitDecompositionEventDescriptorDLCType =>
            val positive = sortedEventDbs.head.outcomeOpt.get == "+"
            val digits = sortedEventDbs.tail.map { eventDb =>
              eventDb.outcomeOpt.get.toInt
            }
            SignedNumericOutcome(positive, digits)
          case _: UnsignedDigitDecompositionEventDescriptor |
              _: UnsignedDigitDecompositionEventDescriptorDLCType =>
            val digits = sortedEventDbs.map { eventDb =>
              eventDb.outcomeOpt.get.toInt
            }
            UnsignedNumericOutcome(digits)
        }
        CompletedDigitDecompositionV0OracleEvent(
          eventDb.pubkey,
          OrderedNonces.fromUnsorted(sortedEventDbs.map(_.nonce)),
          eventDb.eventName,
          eventDb.signingVersion,
          eventDb.maturationTime,
          eventDb.announcementSignature,
          decomp,
          metadataOpt,
          dlcOutcome,
          attestations,
          Some(eventDbs)
        )
    }
  }

  /** Verifies if the given attestations sign the outcomes of the given oracle announcement.
    */
  def verifyAttestations(
      announcement: BaseOracleAnnouncement,
      attestationTLV: OracleAttestmentTLV,
      signingVersion: SigningVersion): Boolean = {
    announcement match {
      case announcementV0TLV: OracleAnnouncementV0TLV =>
        attestationTLV match {
          case attestationsV0: OracleAttestmentV0TLV =>
            verifyAttestationsV0(announcement = announcementV0TLV,
                                 attestationTLV = attestationsV0,
                                 signingVersion = signingVersion)
          case schnorrAttestation: SchnorrAttestationTLV =>
            sys.error(
              s"Cannot have schnorr attestation from v1 of spec with old announcement format, got=$announcementV0TLV attestation=$schnorrAttestation")
        }
      case announcementV1TLV: OracleAnnouncementV1TLV =>
        attestationTLV match {
          case attestationsV0: OracleAttestmentV0TLV =>
            sys.error(
              s"Cannot have attestationv0 from v0 of spec with new announcement format, got=$announcementV1TLV attestation=$attestationsV0")
          case schnorrAttestationTLV: SchnorrAttestationTLV =>
            verifyAttestationsV1(announcementV1TLV = announcementV1TLV,
                                 schnorrAttestation = schnorrAttestationTLV,
                                 signingVersion = signingVersion)
        }
    }
  }

  private def verifyAttestationsV0(
      announcement: OracleAnnouncementV0TLV,
      attestationTLV: OracleAttestmentV0TLV,
      signingVersion: SigningVersion): Boolean = {
    val tlvOutcomes = attestationTLV.outcomes
    val attestations = attestationTLV match {
      case v0: OracleAttestmentV0TLV =>
        v0.unsortedSignatures
    }
    val nonces = announcement.eventTLV match {
      case v0: OracleEventV0TLV =>
        v0.nonces
    }
    verifyAttestationHelper(
      tlvOutcomes = tlvOutcomes,
      attestations = attestations,
      nonces = nonces,
      announcement = announcement,
      attestationPubKey = announcement.announcementPublicKey,
      signingVersion = signingVersion
    )
  }

  private def verifyAttestationsV1(
      announcementV1TLV: OracleAnnouncementV1TLV,
      schnorrAttestation: SchnorrAttestationTLV,
      signingVersion: SigningVersion): Boolean = {
    val tlvOutcomes = schnorrAttestation.outcomes
    val attestations = schnorrAttestation.sigs
    val nonces = announcementV1TLV.nonces.head
    verifyAttestationHelper(
      tlvOutcomes = tlvOutcomes,
      attestations = attestations,
      nonces = nonces,
      announcement = announcementV1TLV,
      attestationPubKey = announcementV1TLV.metadata.attestationPublicKey,
      signingVersion = signingVersion
    )
  }

  private def verifyAttestationHelper(
      tlvOutcomes: Vector[NormalizedString],
      attestations: OrderedSchnorrSignatures,
      nonces: OrderedNonces,
      announcement: BaseOracleAnnouncement,
      attestationPubKey: SchnorrPublicKey,
      signingVersion: SigningVersion): Boolean = {
    if (
      nonces.size != attestations.size ||
      nonces != attestations.map(_.rx)
    ) {
      false
    } else {
      announcement.eventTLV.eventDescriptor match {
        case enum: EnumEventDescriptorV0TLV =>
          verifyEnumAttestation(
            tlvOutcomes = tlvOutcomes,
            attestations = attestations,
            enumEventDescriptor = enum,
            attestationPubKey = announcement.announcementPublicKey,
            signingVersion = signingVersion
          )
        case enum: EnumEventDescriptorDLCSubType =>
          verifyEnumAttestation(tlvOutcomes = tlvOutcomes,
                                attestations = attestations,
                                enumEventDescriptor = enum,
                                attestationPubKey = attestationPubKey,
                                signingVersion = signingVersion)

        case dd: DigitDecompositionEventDescriptorV0TLV =>
          verifyDigitDecompAttestation(tlvOutcomes = tlvOutcomes,
                                       attestations = attestations,
                                       dd = dd,
                                       attestationPubKey = attestationPubKey,
                                       signingVersion = signingVersion)
        case dd: DigitDecompositionEventDescriptorDLCType =>
          verifyDigitDecompAttestation(tlvOutcomes = tlvOutcomes,
                                       attestations = attestations,
                                       dd = dd,
                                       attestationPubKey = attestationPubKey,
                                       signingVersion = signingVersion)
      }
    }
  }

  private def verifyEnumAttestation(
      tlvOutcomes: Vector[NormalizedString],
      attestations: OrderedSchnorrSignatures,
      enumEventDescriptor: BaseEnumEventDescriptor,
      attestationPubKey: SchnorrPublicKey,
      signingVersion: SigningVersion): Boolean = {
    require(attestations.size == 1)

    val sig = attestations.head
    enumEventDescriptor.outcomes.exists { outcome =>
      val attestationType = EnumAttestation(outcome)
      val hash =
        signingVersion.calcOutcomeHash(attestationType.bytes)
      attestationPubKey.verify(hash, sig) && outcome == tlvOutcomes.head
    }
  }

  private def verifyDigitDecompAttestation(
      tlvOutcomes: Vector[NormalizedString],
      attestations: OrderedSchnorrSignatures,
      dd: BaseNumericEventDescriptorTLV,
      attestationPubKey: SchnorrPublicKey,
      signingVersion: SigningVersion): Boolean = {
    require(attestations.nonEmpty)

    val (validSign, attestationsToVerify, outcomesToVerify) =
      dd match {
        case _: SignedDigitDecompositionEventDescriptor |
            _: SignedDigitDecompositionEventDescriptorDLCType =>
          val signOutcomes = Vector(DigitDecompositionSignAttestation(true),
                                    DigitDecompositionSignAttestation(false))

          val validSign = signOutcomes.exists { attestationType =>
            val hash =
              signingVersion.calcOutcomeHash(attestationType.bytes)
            attestationPubKey.verify(
              hash,
              attestations.head) && tlvOutcomes.head.toString == attestationType.outcomeString
          }

          (validSign, attestations.tail, tlvOutcomes.tail)
        case _: UnsignedDigitDecompositionEventDescriptor |
            _: UnsignedDigitDecompositionEventDescriptorDLCType =>
          (true, attestations, tlvOutcomes)
      }

    val digitOutcomes =
      0.until(dd.base.toInt)
        .map(DigitDecompositionAttestation.apply)

    val validDigits =
      attestationsToVerify.zip(outcomesToVerify).forall { case (sig, outcome) =>
        digitOutcomes.exists { attestationType =>
          val hash =
            signingVersion.calcOutcomeHash(attestationType.bytes)
          attestationPubKey.verify(
            hash,
            sig) && attestationType.outcomeString == outcome.toString
        }
      }

    validSign && validDigits
  }
}
