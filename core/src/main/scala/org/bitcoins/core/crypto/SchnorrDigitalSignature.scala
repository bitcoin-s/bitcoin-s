package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

case class SchnorrDigitalSignature(rx: SchnorrNonce, sig: ByteVector)
    extends NetworkElement {
  require(sig.length == 32, "s value must be 32 bytes")
  override def bytes: ByteVector = rx.bytes ++ sig
}

object SchnorrDigitalSignature extends Factory[SchnorrDigitalSignature] {
  override def fromBytes(bytes: ByteVector): SchnorrDigitalSignature = {
    SchnorrDigitalSignature(SchnorrNonce(bytes.take(32)), bytes.drop(32))
  }
}
