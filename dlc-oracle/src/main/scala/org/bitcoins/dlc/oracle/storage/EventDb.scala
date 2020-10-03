package org.bitcoins.dlc.oracle.storage

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.crypto._

case class EventDb(
    nonce: SchnorrNonce,
    pubkey: SchnorrPublicKey,
    eventName: String,
    numOutcomes: Long,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    attestationOpt: Option[FieldElement]) {

  lazy val sigOpt: Option[SchnorrDigitalSignature] =
    attestationOpt.map(SchnorrDigitalSignature(nonce, _))
}
