package org.bitcoins.core.dlc.oracle

import org.bitcoins.crypto.{FieldElement, SchnorrDigitalSignature, SchnorrNonce}

case class NonceSignaturePairDb(
    announcementId: Long,
    nonce: SchnorrNonce,
    nonceProof: SchnorrDigitalSignature,
    attestationOpt: Option[FieldElement],
    outcomeOpt: Option[String]) {
  require(attestationOpt.isDefined == outcomeOpt.isDefined,
          s"Attestation must be present if outcome is present")

  val signatureOpt: Option[SchnorrDigitalSignature] = {
    attestationOpt.map { attestation =>
      SchnorrDigitalSignature(nonce, attestation)
    }
  }

  val nonceSignaturePair: NonceSignaturePair = {
    NonceSignaturePair(nonce = nonce, nonceProof = nonceProof)
  }
}
