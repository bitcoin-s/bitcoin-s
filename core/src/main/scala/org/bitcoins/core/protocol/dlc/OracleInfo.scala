package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto._

sealed trait OracleInfo extends TLVSerializable[OracleInfoTLV] {
  def threshold: Int
  def numOracles: Int
  def singleOracleInfos: Vector[SingleOracleInfo]
}

sealed trait EnumOracleInfo extends OracleInfo {
  override def singleOracleInfos: Vector[EnumSingleOracleInfo]
}

sealed trait NumericOracleInfo extends OracleInfo {
  override def singleOracleInfos: Vector[NumericSingleOracleInfo]
}

object OracleInfo
    extends TLVDeserializable[OracleInfoTLV, OracleInfo](OracleInfoTLV) {

  override def fromTLV(tlv: OracleInfoTLV): OracleInfo = {
    tlv match {
      case tlv: OracleInfoV0TLV => SingleOracleInfo.fromTLV(tlv)
      case tlv: OracleInfoV1TLV => ExactMultiOracleInfo.fromTLV(tlv)
      case tlv: OracleInfoV2TLV => NumericMultiOracleInfo.fromTLV(tlv)
    }
  }
}

sealed trait SingleOracleInfo
    extends OracleInfo
    with TLVSerializable[OracleInfoV0TLV] {
  override val numOracles: Int = 1
  override val threshold: Int = 1

  override def singleOracleInfos: Vector[this.type] = Vector(this)

  def announcement: OracleAnnouncementTLV
  def publicKey: SchnorrPublicKey = announcement.publicKey

  /** The oracle's pre-committed nonces, in the correct order */
  def nonces: Vector[SchnorrNonce] = announcement.eventTLV.nonces

  /** The order of the given sigs should correspond to the given outcome. */
  def verifySigs(outcome: DLCOutcomeType, sigs: OracleSignatures): Boolean

  /** Computes the signature point (aka signature anticipation) for a given outcome.
    * This point is used for adaptor signing.
    */
  def sigPoint(outcome: DLCOutcomeType): ECPublicKey = {
    publicKey.computeSigPoint(outcome.serialized, nonces)
  }

  override def toTLV: OracleInfoV0TLV = OracleInfoV0TLV(announcement)
}

object SingleOracleInfo
    extends TLVDeserializable[OracleInfoV0TLV, SingleOracleInfo](
      OracleInfoV0TLV) {

  def apply(announcement: OracleAnnouncementTLV): SingleOracleInfo = {
    announcement.eventTLV.eventDescriptor match {
      case _: EnumEventDescriptorV0TLV =>
        EnumSingleOracleInfo(announcement)
      case _: NumericEventDescriptorTLV =>
        NumericSingleOracleInfo(announcement)
    }
  }

  def apply(tlv: OracleInfoV0TLV): SingleOracleInfo = {
    SingleOracleInfo(tlv.announcement)
  }

  override def fromTLV(tlv: OracleInfoV0TLV): SingleOracleInfo = {
    SingleOracleInfo(tlv)
  }
}

case class EnumSingleOracleInfo(announcement: OracleAnnouncementTLV)
    extends SingleOracleInfo
    with EnumOracleInfo {
  require(announcement.eventTLV.eventDescriptor
            .isInstanceOf[EnumEventDescriptorV0TLV],
          s"Enum OracleInfo requires EnumEventDescriptor, $announcement")

  val nonce: SchnorrNonce = announcement.eventTLV.nonces.head

  override def verifySigs(
      outcome: DLCOutcomeType,
      sigs: OracleSignatures): Boolean = {
    outcome match {
      case EnumOutcome(outcome) =>
        sigs match {
          case _: NumericOracleSignatures =>
            throw new IllegalArgumentException(
              s"Expected one signature, got $sigs")
          case EnumOracleSignature(_, sig) =>
            if (sig.rx != nonce) {
              throw new IllegalArgumentException(
                s"Expected R value of $nonce, got $sig")
            } else {
              publicKey.verify(CryptoUtil.sha256DLCAttestation(outcome).bytes,
                               sig)
            }
        }
      case UnsignedNumericOutcome(_) =>
        throw new IllegalArgumentException(
          s"Expected EnumOutcome, got $outcome")
    }
  }
}

object EnumSingleOracleInfo
    extends TLVDeserializable[OracleInfoV0TLV, EnumSingleOracleInfo](
      OracleInfoV0TLV) {

  def dummyForKeys(
      privKey: ECPrivateKey,
      nonce: SchnorrNonce,
      events: Vector[EnumOutcome]): EnumSingleOracleInfo = {
    EnumSingleOracleInfo(
      OracleAnnouncementV0TLV
        .dummyForEventsAndKeys(privKey, nonce, events))
  }

  override def fromTLV(tlv: OracleInfoV0TLV): EnumSingleOracleInfo = {
    EnumSingleOracleInfo(tlv.announcement)
  }
}

case class NumericSingleOracleInfo(announcement: OracleAnnouncementTLV)
    extends SingleOracleInfo
    with NumericOracleInfo {
  require(announcement.eventTLV.eventDescriptor
            .isInstanceOf[NumericEventDescriptorTLV],
          s"Numeric OracleInfo requires NumericEventDescriptor, $announcement")

  override def verifySigs(
      outcome: DLCOutcomeType,
      sigs: OracleSignatures): Boolean = {
    require(sigs.nonEmpty, "At least one signature is required")
    require(
      sigs.length <= nonces.length,
      s"Too many signatures (expected at most ${nonces.length}), got $sigs")

    outcome match {
      case EnumOutcome(_) =>
        throw new IllegalArgumentException(
          s"Expected numeric outcome, got $outcome")
      case UnsignedNumericOutcome(digits) =>
        digits
          .zip(sigs.take(digits.length).zip(nonces.take(digits.length)))
          .foldLeft(digits.length <= sigs.length) {
            case (result, (digit, (sig, nonce))) =>
              require(sig.rx == nonce,
                      s"Unexpected nonce in ${sig.hex}, expected ${nonce.hex}")

              result && publicKey.verify(
                CryptoUtil.sha256DLCAttestation(digit.toString).bytes,
                sig)
          }
    }
  }
}

object NumericSingleOracleInfo {

  def dummyForKeys(
      privKey: ECPrivateKey,
      nonces: Vector[SchnorrNonce]): NumericSingleOracleInfo = {
    NumericSingleOracleInfo(
      OracleAnnouncementV0TLV.dummyForKeys(privKey, nonces))
  }
}

sealed trait MultiOracleInfo[+T <: SingleOracleInfo]
    extends OracleInfo
    with TLVSerializable[MultiOracleInfoTLV] {
  override def numOracles: Int = announcements.length

  def announcements: Vector[OracleAnnouncementTLV]

  require(
    announcements.length >= threshold,
    s"Cannot have threshold ($threshold) above the number of oracles (${announcements.length})")

  // Override this with a val to invoke requirements
  def singleOracleInfos: Vector[T]
}

sealed trait ExactMultiOracleInfo[+T <: SingleOracleInfo]
    extends MultiOracleInfo[T]
    with TLVSerializable[OracleInfoV1TLV] {

  override def toTLV: OracleInfoV1TLV =
    OracleInfoV1TLV(threshold, announcements)
}

object ExactMultiOracleInfo
    extends TLVDeserializable[
      OracleInfoV1TLV,
      ExactMultiOracleInfo[SingleOracleInfo]](OracleInfoV1TLV) {

  def apply(tlv: OracleInfoV1TLV): ExactMultiOracleInfo[SingleOracleInfo] = {
    tlv.oracles.head.eventTLV.eventDescriptor match {
      case _: EnumEventDescriptorV0TLV =>
        EnumMultiOracleInfo(tlv.threshold, tlv.oracles)
      case _: NumericEventDescriptorTLV =>
        NumericExactMultiOracleInfo(tlv.threshold, tlv.oracles)
    }
  }

  override def fromTLV(
      tlv: OracleInfoV1TLV): ExactMultiOracleInfo[SingleOracleInfo] = {
    ExactMultiOracleInfo(tlv)
  }
}

case class EnumMultiOracleInfo(
    threshold: Int,
    announcements: Vector[OracleAnnouncementTLV])
    extends ExactMultiOracleInfo[EnumSingleOracleInfo]
    with EnumOracleInfo {

  override val singleOracleInfos: Vector[EnumSingleOracleInfo] =
    announcements.map(EnumSingleOracleInfo.apply)
}

case class NumericExactMultiOracleInfo(
    threshold: Int,
    announcements: Vector[OracleAnnouncementTLV])
    extends ExactMultiOracleInfo[NumericSingleOracleInfo]
    with NumericOracleInfo {

  val singleOracleInfos: Vector[NumericSingleOracleInfo] =
    announcements.map(NumericSingleOracleInfo.apply)
}

case class NumericMultiOracleInfo(
    threshold: Int,
    announcements: Vector[OracleAnnouncementTLV],
    maxErrorExp: Int,
    minFailExp: Int,
    maximizeCoverage: Boolean)
    extends MultiOracleInfo[NumericSingleOracleInfo]
    with TLVSerializable[OracleInfoV2TLV]
    with NumericOracleInfo {

  override val singleOracleInfos: Vector[NumericSingleOracleInfo] =
    announcements.map(NumericSingleOracleInfo.apply)

  override def toTLV: OracleInfoV2TLV = {
    OracleInfoV2TLV(
      threshold,
      announcements,
      OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage))
  }
}

object NumericMultiOracleInfo
    extends TLVDeserializable[OracleInfoV2TLV, NumericMultiOracleInfo](
      OracleInfoV2TLV) {

  def apply(
      threshold: Int,
      announcements: Vector[OracleAnnouncementTLV],
      params: OracleParamsTLV): NumericMultiOracleInfo = {
    params match {
      case OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage) =>
        NumericMultiOracleInfo(threshold,
                               announcements,
                               maxErrorExp,
                               minFailExp,
                               maximizeCoverage)
    }
  }

  override def fromTLV(tlv: OracleInfoV2TLV): NumericMultiOracleInfo = {
    NumericMultiOracleInfo(tlv.threshold, tlv.oracles, tlv.params)
  }
}
