package org.bitcoins.core.dlc.oracle

import org.bitcoins.crypto.{
  SchnorrDigitalSignature,
  SchnorrNonce,
  SchnorrPublicKey
}

case class NonceSignaturePair(
    nonce: SchnorrNonce,
    nonceSignature: SchnorrDigitalSignature) {

  def verify(pubKey: SchnorrPublicKey): Boolean = {
    pubKey.verify(nonce.bytes, nonceSignature)
  }
}
