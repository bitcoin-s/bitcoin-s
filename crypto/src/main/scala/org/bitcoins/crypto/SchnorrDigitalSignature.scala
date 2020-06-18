package org.bitcoins.crypto

import scodec.bits.ByteVector

case class SchnorrDigitalSignature(rx: SchnorrNonce, sig: FieldElement)
    extends NetworkElement {
  override def bytes: ByteVector = rx.bytes ++ sig.bytes
}

object SchnorrDigitalSignature extends Factory[SchnorrDigitalSignature] {

  override def fromBytes(bytes: ByteVector): SchnorrDigitalSignature = {
    require(bytes.length == 64,
            s"SchnorrDigitalSignature must be exactly 64 bytes, got $bytes")
    SchnorrDigitalSignature(SchnorrNonce(bytes.take(32)),
                            FieldElement(bytes.drop(32)))
  }
}
