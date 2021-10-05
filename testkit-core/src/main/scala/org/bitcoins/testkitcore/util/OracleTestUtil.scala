package org.bitcoins.testkitcore.util

import org.bitcoins.core.api.dlcoracle.{CompletedOracleEvent, OracleEvent}
import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv.{
  EnumEventDescriptorV0TLV,
  NormalizedString,
  OracleAttestmentV0TLV
}
import org.bitcoins.crypto.{
  CryptoUtil,
  ECPrivateKey,
  ECPublicKey,
  SchnorrDigitalSignature,
  Sha256Digest
}

import java.time.Instant

object OracleTestUtil {
  val testAddressStr = "bc1qvrctqwa6g70z5vtxsyft7xvsyyt749trlm80al"
  val testAddress: Bech32Address = Bech32Address.fromString(testAddressStr)

  val kVal: ECPrivateKey = ECPrivateKey.fromHex(
    "447d4457dfff21354d56cb1b62b2ab6e5964c5ef93e6d74ae3b30dc83b89b6a5")

  val dummyPrivKey: ECPrivateKey = ECPrivateKey.fromHex(
    "f04671ab68f3fefbeaa344c49149748f722287a81b19cd956b2332d07b8f6853")

  val dummyKey: ECPublicKey = dummyPrivKey.publicKey

  val outcome: NormalizedString = EnumEventDescriptorV0TLV.dummy.outcomes.head

  val hash: Sha256Digest = CryptoUtil.sha256DLCAttestation(outcome)

  val sig: SchnorrDigitalSignature =
    dummyPrivKey.schnorrSignWithNonce(hash.bytes, kVal)

  val dummyEventDb: EventDb = EventDb(
    nonce = kVal.schnorrNonce,
    pubkey = dummyKey.schnorrPublicKey,
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

  val dummyOracleEvent: CompletedOracleEvent = OracleEvent
    .fromEventDbs(Vector(dummyEventDb))
    .asInstanceOf[CompletedOracleEvent]

  val dummyAttestmentTLV: OracleAttestmentV0TLV =
    dummyOracleEvent.oracleAttestmentV0TLV
}
