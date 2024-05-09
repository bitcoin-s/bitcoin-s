package org.bitcoins.crypto

import scodec.bits.ByteVector

case class ECAdaptorSignature(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 162,
          s"Adaptor signature must be 162 bytes, got $bytes")

  val (adaptedSig: ByteVector, dleqProof: ByteVector) = bytes.splitAt(98)

  val tweakedNonce: ECPublicKey = ECPublicKey(adaptedSig.take(33))
  val untweakedNonce: ECPublicKey = ECPublicKey(adaptedSig.drop(33).take(33))
  val adaptedS: FieldElement = FieldElement(adaptedSig.drop(66))

  require(!adaptedS.isZero, "Adapted signature cannot be zero.")

  val dleqProofE: FieldElement = FieldElement(dleqProof.take(32))
  val dleqProofS: FieldElement = FieldElement(dleqProof.drop(32))
}

object ECAdaptorSignature extends Factory[ECAdaptorSignature] {

  def fromBytes(bytes: ByteVector): ECAdaptorSignature = {
    new ECAdaptorSignature(bytes)
  }

  def apply(
      tweakedNonce: ECPublicKey,
      untweakedNonce: ECPublicKey,
      adaptedS: FieldElement,
      dleqProofE: FieldElement,
      dleqProofS: FieldElement): ECAdaptorSignature = {
    fromBytes(
      tweakedNonce.bytes ++ untweakedNonce.bytes ++
        adaptedS.bytes ++ dleqProofE.bytes ++ dleqProofS.bytes
    )
  }

  lazy val dummy: ECAdaptorSignature = {
    ECAdaptorSignature(
      ECPublicKey.freshPublicKey,
      ECPublicKey.freshPublicKey,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPrivateKey.freshPrivateKey.fieldElement
    )
  }
}
