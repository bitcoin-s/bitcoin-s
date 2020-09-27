package org.bitcoins.dlc.oracle.storage

import org.bitcoins.crypto.{FieldElement, SchnorrDigitalSignature, SchnorrNonce}
import org.bitcoins.dlc.oracle.SigningVersion

case class EventDb(
    nonce: SchnorrNonce,
    label: String,
    numOutcomes: Long,
    signingVersion: SigningVersion,
    attestationOpt: Option[FieldElement]) {

  lazy val sigOpt: Option[SchnorrDigitalSignature] =
    attestationOpt.map(SchnorrDigitalSignature(nonce, _))
}
