package org.bitcoins.dlc.oracle.storage

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.core.protocol.tlv.EventDescriptorTLV
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.OracleEvent

case class EventDb(
    nonce: SchnorrNonce,
    pubkey: SchnorrPublicKey,
    nonceIndex: Int,
    eventName: String,
    numOutcomes: Long,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    attestationOpt: Option[FieldElement],
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: EventDescriptorTLV) {

  lazy val sigOpt: Option[SchnorrDigitalSignature] =
    attestationOpt.map(SchnorrDigitalSignature(nonce, _))

  lazy val toOracleEvent: OracleEvent = OracleEvent.fromEventDbs(Vector(this))
}
