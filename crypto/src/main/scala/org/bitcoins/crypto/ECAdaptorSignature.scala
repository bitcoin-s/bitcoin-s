package org.bitcoins.crypto

import scodec.bits.ByteVector

case class ECAdaptorSignature(bytes: ByteVector) extends NetworkElement {
  require(
    bytes.length == 162,
    s"Adaptor signature must have 65 byte sig and 97 byte dleq proof, got $bytes")

  val (adaptedSig: ByteVector, dleqProof: ByteVector) = bytes.splitAt(65)

  val tweakedNonce: ECPublicKey =
    ECAdaptorSignature.deserializePoint(adaptedSig.take(33))
  val adaptedS: FieldElement = FieldElement(adaptedSig.drop(33))

  require(!adaptedS.isZero, "Adapted signature cannot be zero.")

  val untweakedNonce: ECPublicKey =
    ECAdaptorSignature.deserializePoint(dleqProof.take(33))
  val dleqProofS: FieldElement = FieldElement(dleqProof.drop(33).take(32))
  val dleqProofE: FieldElement = FieldElement(dleqProof.drop(65))

  require(CryptoUtil.isFullyValidWithBouncyCastle(tweakedNonce.bytes),
          s"Tweaked nonce (R) must be a valid public key: $tweakedNonce")
  require(CryptoUtil.isFullyValidWithBouncyCastle(untweakedNonce.bytes),
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
      serializePoint(tweakedNonce) ++ adaptedS.bytes ++ serializePoint(
        untweakedNonce) ++ dleqProofS.bytes ++ dleqProofE.bytes
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

  def serializePoint(point: ECPublicKey): ByteVector = {
    val (sign, xCoor) = point.bytes.splitAt(1)
    sign.map(b => (b & 0x01).toByte) ++ xCoor
  }

  def deserializePoint(point: ByteVector): ECPublicKey = {
    val (sign, xCoor) = point.splitAt(1)
    ECPublicKey(sign.map(b => (b | 0x02).toByte) ++ xCoor)
  }
}
