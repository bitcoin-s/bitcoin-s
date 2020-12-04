package org.bitcoins.commons

import org.bitcoins.core.number.{Int32, UInt16, UInt32}
import org.bitcoins.core.protocol.tlv.{
  EnumEventDescriptorV0TLV,
  NormalizedString,
  OracleAnnouncementV0TLV,
  OracleEventV0TLV,
  RangeEventDescriptorV0TLV,
  SignedDigitDecompositionEventDescriptor,
  UnsignedDigitDecompositionEventDescriptor
}
import org.bitcoins.crypto.{DoubleSha256Digest, ECPrivateKey, SchnorrNonce}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import play.api.libs.json.Json
import org.bitcoins.commons.serializers.JsonSerializers._

class TLVJsonSerializersTest extends BitcoinSUnitTest {
  behavior of "SerializedPSBT"

  val outcomes = Vector("Democrat_win", "Republican_win", "other")
  val enumEventDescriptorV0TLV = EnumEventDescriptorV0TLV(outcomes)

  val rangeEventDescriptorV0TLV =
    RangeEventDescriptorV0TLV(start = Int32(-2),
                              count = UInt32(4),
                              step = UInt16.one,
                              unit = "test_unit",
                              precision = Int32.zero)

  val unsignedDigitDecompositionEventDescriptor =
    UnsignedDigitDecompositionEventDescriptor(base = UInt16(10),
                                              numDigits = UInt16(1),
                                              unit = "BTC/USD",
                                              precision = Int32.zero)

  val signedDigitDecompositionEventDescriptor =
    SignedDigitDecompositionEventDescriptor(base = UInt16(10),
                                            numDigits = UInt16(1),
                                            unit = "BTC/USD",
                                            precision = Int32.zero)

  val privateKey = ECPrivateKey.freshPrivateKey

  val publicKey = privateKey.publicKey

  val schnorrPublicKey = privateKey.schnorrPublicKey

  val schnorrDigitalSignature =
    privateKey.schnorrSign(DoubleSha256Digest.empty.bytes)

  it must "correctly decode an enum oracle announcement" in {

    val event = OracleEventV0TLV(
      nonces = Vector(SchnorrNonce(publicKey.bytes)),
      eventMaturityEpoch = UInt32(1234567890),
      eventDescriptor = enumEventDescriptorV0TLV,
      eventId = NormalizedString("event-id")
    )

    val announcement =
      OracleAnnouncementV0TLV(schnorrDigitalSignature, schnorrPublicKey, event)

    val json = Json.toJson(announcement)

    assert(
      (json \ "announcementSignature")
        .validate[String]
        .get == announcement.announcementSignature.hex)
    assert(
      (json \ "publicKey")
        .validate[String]
        .get == announcement.publicKey.hex)
    assert(
      (json \ "eventTLV" \ "eventId")
        .validate[String]
        .get == event.eventId.normStr)
    assert(
      (json \ "eventTLV" \ "eventMaturityEpoch")
        .validate[Long]
        .get == event.eventMaturityEpoch.toLong)
    assert(
      (json \ "eventTLV" \ "eventDescriptor" \ "outcomes")
        .validate[Vector[String]]
        .get == enumEventDescriptorV0TLV.outcomes.map(_.normStr))
  }

  it must "correctly decode a signed digit decomposition oracle announcement" in {

    val event = OracleEventV0TLV(
      nonces =
        Vector(SchnorrNonce(publicKey.bytes), SchnorrNonce(publicKey.bytes)),
      eventMaturityEpoch = UInt32(1234567890),
      eventDescriptor = signedDigitDecompositionEventDescriptor,
      eventId = NormalizedString("event-id")
    )

    val announcement =
      OracleAnnouncementV0TLV(schnorrDigitalSignature, schnorrPublicKey, event)

    val json = Json.toJson(announcement)

    assert(
      (json \ "announcementSignature")
        .validate[String]
        .get == announcement.announcementSignature.hex)
    assert(
      (json \ "publicKey")
        .validate[String]
        .get == announcement.publicKey.hex)
    assert(
      (json \ "eventTLV" \ "eventId")
        .validate[String]
        .get == event.eventId.normStr)
    assert(
      (json \ "eventTLV" \ "eventMaturityEpoch")
        .validate[Long]
        .get == event.eventMaturityEpoch.toLong)
    assert(
      (json \ "eventTLV" \ "eventDescriptor" \ "base")
        .validate[Int]
        .get == signedDigitDecompositionEventDescriptor.base.toInt)
    assert(
      (json \ "eventTLV" \ "eventDescriptor" \ "unit")
        .validate[String]
        .get == signedDigitDecompositionEventDescriptor.unit.normStr)
  }

  it must "correctly decode a unsigned digit decomposition oracle announcement" in {

    val event = OracleEventV0TLV(
      nonces = Vector(SchnorrNonce(publicKey.bytes)),
      eventMaturityEpoch = UInt32(1234567890),
      eventDescriptor = unsignedDigitDecompositionEventDescriptor,
      eventId = NormalizedString("event-id")
    )

    val announcement =
      OracleAnnouncementV0TLV(schnorrDigitalSignature, schnorrPublicKey, event)

    val json = Json.toJson(announcement)

    assert(
      (json \ "announcementSignature")
        .validate[String]
        .get == announcement.announcementSignature.hex)
    assert(
      (json \ "publicKey")
        .validate[String]
        .get == announcement.publicKey.hex)
    assert(
      (json \ "eventTLV" \ "eventId")
        .validate[String]
        .get == event.eventId.normStr)
    assert(
      (json \ "eventTLV" \ "eventMaturityEpoch")
        .validate[Long]
        .get == event.eventMaturityEpoch.toLong)
    assert(
      (json \ "eventTLV" \ "eventDescriptor" \ "base")
        .validate[Int]
        .get == signedDigitDecompositionEventDescriptor.base.toInt)
    assert(
      (json \ "eventTLV" \ "eventDescriptor" \ "unit")
        .validate[String]
        .get == signedDigitDecompositionEventDescriptor.unit.normStr)
  }
}
