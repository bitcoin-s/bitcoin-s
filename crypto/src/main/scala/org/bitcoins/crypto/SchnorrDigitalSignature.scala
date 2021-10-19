package org.bitcoins.crypto

import scodec.bits.ByteVector

case class SchnorrDigitalSignature(rx: SchnorrNonce, sig: FieldElement)
    extends NetworkElement {
  override def bytes: ByteVector = rx.bytes ++ sig.bytes
}

object SchnorrDigitalSignature extends Factory[SchnorrDigitalSignature] {

  //If the sig is 65 bytes long, return sig[64] ≠ 0x00[20] and
  // Verify(q, hashTapSighash(0x00 || SigMsg(sig[64], 0)), sig[0:64]).
  override def fromBytes(bytes: ByteVector): SchnorrDigitalSignature = {
    require(bytes.length == 64,
            s"SchnorrDigitalSignature must be exactly 64 bytes, got $bytes")
    SchnorrDigitalSignature(SchnorrNonce(bytes.take(32)),
                            FieldElement(bytes.drop(32)))
  }

  lazy val dummy: SchnorrDigitalSignature =
    SchnorrDigitalSignature(FieldElement.one.getPublicKey.schnorrNonce,
                            FieldElement.one)
}
