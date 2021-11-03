package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.dlc.oracle
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrNonce}

case class NonceSignaturePairDb(
    id: Long,
    nonce: SchnorrNonce,
    signature: SchnorrDigitalSignature) {

  val nonceSignaturePair: NonceSignaturePair = {
    oracle.NonceSignaturePair(nonce = nonce, nonceSignature = signature)
  }
}
