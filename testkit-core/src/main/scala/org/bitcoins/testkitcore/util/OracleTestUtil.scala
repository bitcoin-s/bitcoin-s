package org.bitcoins.testkitcore.util

import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.api.dlcoracle.{
  CompletedDigitDecompositionV0OracleEvent,
  CompletedEnumV0OracleEvent,
  OracleEvent
}
import org.bitcoins.core.number.{Int32, UInt16, UInt32, UInt8}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.crypto._

import java.time.Instant

object OracleTestUtil {
  val testAddressStr = "bc1qvrctqwa6g70z5vtxsyft7xvsyyt749trlm80al"
  val testAddress: Bech32Address = Bech32Address.fromString(testAddressStr)

  val kVal: ECPrivateKey = ECPrivateKey.fromHex(
    "447d4457dfff21354d56cb1b62b2ab6e5964c5ef93e6d74ae3b30dc83b89b6a5")

  val kVal1: ECPrivateKey = ECPrivateKey.fromHex(
    "828dd2143f1d9bab2b077649a14c290cdadf2531c03351da0294643827f69690")

  val dummyPrivKey: ECPrivateKey = ECPrivateKey.fromHex(
    "f04671ab68f3fefbeaa344c49149748f722287a81b19cd956b2332d07b8f6853")

  val attestationPrivKey = ECPrivateKey.fromHex(
    "1a419d92f6b78502a03449e01b36a30c3d33ef8c1ff6e1a37de6845a9e425b63")
  val attesatationPubKey = attestationPrivKey.schnorrPublicKey

  val dummyKey: ECPublicKey = dummyPrivKey.publicKey

  val outcome: NormalizedString = EnumEventDescriptorV0TLV.dummy.outcomes.head

  val hash: Sha256Digest = CryptoUtil.sha256DLCAttestation(outcome)

  val sig: SchnorrDigitalSignature =
    attestationPrivKey.schnorrSignWithNonce(hash.bytes, kVal)

  val dummyEventDb: EventDb = EventDb(
    nonce = kVal.schnorrNonce,
    pubkey = attesatationPubKey,
    nonceIndex = 0,
    eventName = "id",
    numOutcomes = 2,
    signingVersion = SigningVersion.latest,
    maturationTime = Instant.ofEpochSecond(0),
    attestationOpt = Some(sig.sig),
    outcomeOpt = Some(outcome),
    announcementSignature = SchnorrDigitalSignature(
      "1efe41fa42ea1dcd103a0251929dd2b192d2daece8a4ce4d81f68a183b750d92d6f02d796965dc79adf4e7786e08f861a1ecc897afbba2dab9cff6eb0a81937e"),
    eventDescriptorTLV = EnumEventDescriptorV0TLV.dummy
  )

  val numericOutcome = "0"
  val numericOutcomeHash0 = CryptoUtil.sha256DLCAttestation(numericOutcome)
  val numericOutcomeHash1 = CryptoUtil.sha256DLCAttestation(numericOutcome)

  val signNumeric0 =
    attestationPrivKey.schnorrSignWithNonce(numericOutcomeHash0.bytes, kVal)

  val signNumeric1 =
    attestationPrivKey.schnorrSignWithNonce(numericOutcomeHash1.bytes, kVal1)

  val dummyDigitDecomp = UnsignedDigitDecompositionEventDescriptorDLCType(
    baseU8 = UInt8.two,
    numDigits = UInt16.one,
    unit = "test-unit",
    precision = Int32.zero
  )

  private val orderedNonces = OrderedNonces(
    Vector(kVal.schnorrNonce, kVal1.schnorrNonce))

  val dummyNumericEventDb0: EventDb = {
    dummyEventDb.copy(nonce = orderedNonces.toVector.head,
                      eventDescriptorTLV = dummyDigitDecomp,
                      outcomeOpt = Some(numericOutcome),
                      attestationOpt = Some(signNumeric0.sig))
  }

  val dummyNumericEventDb1: EventDb = {
    dummyNumericEventDb0.copy(
      nonce = orderedNonces.toVector(1),
      nonceIndex = 1,
      eventDescriptorTLV = dummyDigitDecomp,
      outcomeOpt = Some(numericOutcome),
      attestationOpt = Some(signNumeric1.sig)
    )
  }

  val dummyMetadata: OracleMetadata = {
    val announcementPrivKey = ECPrivateKey.freshPrivateKey
    val announcementPubKey = announcementPrivKey.schnorrPublicKey

    val name = NormalizedString("oracle_name")
    val description = NormalizedString("oracle_description")
    val creationTime: UInt32 = UInt32(Instant.now.getEpochSecond)
    val schnorrAttestation: SchnorrAttestation =
      SchnorrAttestation.build(announcementPrivKey,
                               attesatationPubKey,
                               orderedNonces)
    val metadataSignature: OracleMetadataSignature = OracleMetadataSignature
      .buildSignature(announcementPrivKey = announcementPrivKey,
                      oracleName = name,
                      oracleDescription = description,
                      creationTime = creationTime,
                      schnorrAttestation = schnorrAttestation)

    val metadata = OracleMetadata(announcementPubKey,
                                  name,
                                  description,
                                  creationTime = creationTime,
                                  attestations = schnorrAttestation,
                                  metadataSignature = metadataSignature)
    metadata
  }

  val dummyEnumOracleEventCompleted: CompletedEnumV0OracleEvent = OracleEvent
    .fromCompletedEventDbs(Vector(dummyEventDb), Some(dummyMetadata))
    .asInstanceOf[CompletedEnumV0OracleEvent]

  val dummyNumericOracleEventCompleted: CompletedDigitDecompositionV0OracleEvent = {

    OracleEvent
      .fromCompletedEventDbs(Vector(dummyNumericEventDb0, dummyNumericEventDb1),
                             Some(dummyMetadata))
      .asInstanceOf[CompletedDigitDecompositionV0OracleEvent]
  }

  val dummyAttestmentTLV: OracleAttestmentTLV =
    dummyEnumOracleEventCompleted.oracleAttestmentV0TLV

  val dummyNumericAttestmentTLV: OracleAttestmentTLV = {
    dummyNumericOracleEventCompleted.oracleAttestmentV0TLV
  }
}
