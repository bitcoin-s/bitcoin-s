package org.bitcoins.dlc.oracle.storage

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.crypto.{FieldElement, SchnorrDigitalSignature, SchnorrNonce}

case class EventDb(
    nonce: SchnorrNonce,
    eventName: String,
    numOutcomes: Long,
    signingVersion: SigningVersion,
    attestationOpt: Option[FieldElement]) {

  lazy val sigOpt: Option[SchnorrDigitalSignature] =
    attestationOpt.map(SchnorrDigitalSignature(nonce, _))
}
