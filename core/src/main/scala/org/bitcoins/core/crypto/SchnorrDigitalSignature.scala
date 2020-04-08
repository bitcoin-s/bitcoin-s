package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

// TODO: Replace rx with a nice x-only public key type
case class SchnorrDigitalSignature(rx: ByteVector, sig: ByteVector)
    extends NetworkElement {
  require(rx.length == 32, "R value must be 32 bytes")
  require(sig.length == 32, "s value must be 32 bytes")
  override def bytes: ByteVector = rx ++ sig
}

object SchnorrDigitalSignature extends Factory[SchnorrDigitalSignature] {
  override def fromBytes(bytes: ByteVector): SchnorrDigitalSignature = {
    SchnorrDigitalSignature(bytes.take(32), bytes.drop(32))
  }
}
