package org.bitcoins.crypto

import scodec.bits.ByteVector

case class SchnorrDigitalSignature(
    rx: SchnorrNonce,
    sig: FieldElement,
    hashTypeOpt: Option[HashType])
    extends DigitalSignature {
  override val bytes: ByteVector = rx.bytes ++ sig.bytes
  require(
    bytes.length == 64 || bytes.length == 65,
    s"SchnorrDigitalSignature must be 64/65 bytes in size, got=${bytes.length}")

  override def appendHashType(hashType: HashType): SchnorrDigitalSignature = {
    require(this.hashTypeOpt.isEmpty,
            "Cannot append HashType to signature which already has HashType")

    val bytesWithHashType = bytes.:+(hashType.byte)
    SchnorrDigitalSignature.fromBytes(bytesWithHashType)
  }
}

object SchnorrDigitalSignature extends Factory[SchnorrDigitalSignature] {

  // If the sig is 65 bytes long, return sig[64] ≠ 0x00[20] and
  // Verify(q, hashTapSighash(0x00 || SigMsg(sig[64], 0)), sig[0:64]).
  override def fromBytes(bytes: ByteVector): SchnorrDigitalSignature = {
    require(bytes.length == 64 || bytes.length == 65,
            s"SchnorrDigitalSignature must be exactly 64 bytes, got $bytes")
    val r = bytes.take(32)
    val s = bytes.drop(32).take(32)
    val hashTypeOpt = if (bytes.length == 65) Some(bytes(65)) else None
    SchnorrDigitalSignature(SchnorrNonce(r),
                            FieldElement(s),
                            hashTypeOpt.map(HashType.fromByte))
  }

  lazy val dummy: SchnorrDigitalSignature =
    SchnorrDigitalSignature(FieldElement.one.getPublicKey.schnorrNonce,
                            FieldElement.one,
                            hashTypeOpt = None)
}
