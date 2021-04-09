package org.bitcoins.crypto

import scodec.bits.ByteVector

case class ECAdaptorSignature(bytes: ByteVector) extends NetworkElement {
  require(
    bytes.length == 162,
    s"Adaptor signature must have 65 byte sig and 97 byte dleq proof, got $bytes")

  val (adaptedSig: ByteVector, dleqProof: ByteVector) = bytes.splitAt(98)

  val tweakedNonce: ECPublicKey = ECPublicKey(adaptedSig.take(33))
  val untweakedNonce: ECPublicKey = ECPublicKey(adaptedSig.drop(33).take(33))
  val adaptedS: FieldElement = FieldElement(adaptedSig.drop(66))

  require(!adaptedS.isZero, "Adapted signature cannot be zero.")

  val dleqProofS: FieldElement = FieldElement(dleqProof.take(32))
  val dleqProofE: FieldElement = FieldElement(dleqProof.drop(32))

  require(
    tweakedNonce.bytes.nonEmpty && CryptoUtil.isValidPubKey(tweakedNonce.bytes),
    s"Tweaked nonce (R) must be a valid public key: $tweakedNonce")
  require(untweakedNonce.bytes.nonEmpty && CryptoUtil.isValidPubKey(
            untweakedNonce.bytes),
          s"Untweaked nonce (R') must be a valid public key: $untweakedNonce")
}

object ECAdaptorSignature extends Factory[ECAdaptorSignature] {

  def fromBytes(bytes: ByteVector): ECAdaptorSignature = {
    new ECAdaptorSignature(bytes)
  }

  def apply(
      tweakedNonce: ECPublicKey,
      adaptedS: FieldElement,
      untweakedNonce: ECPublicKey,
      dleqProofS: FieldElement,
      dleqProofE: FieldElement): ECAdaptorSignature = {
    fromBytes(
      tweakedNonce.compressed.bytes ++ adaptedS.bytes ++
        untweakedNonce.compressed.bytes ++ dleqProofS.bytes ++ dleqProofE.bytes
    )
  }

  lazy val dummy: ECAdaptorSignature = {
    ECAdaptorSignature(
      ECPublicKey.freshPublicKey,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPublicKey.freshPublicKey,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPrivateKey.freshPrivateKey.fieldElement
    )
  }
}
