package org.bitcoins.crypto

import scodec.bits.ByteVector

case class SchnorrNonce(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 32, s"Schnorr nonce must be 32 bytes, get $bytes")

  private val schnorrPublicKey: SchnorrPublicKey = new SchnorrPublicKey(bytes)

  val publicKey: ECPublicKey = schnorrPublicKey.publicKey

  def xCoord: FieldElement = {
    FieldElement(bytes)
  }
}

object SchnorrNonce extends Factory[SchnorrNonce] {

  def fromBytes(bytes: ByteVector): SchnorrNonce = {
    new SchnorrNonce(SchnorrPublicKey.fromBytes(bytes).bytes)
  }

  def kFromBipSchnorr(
      privKey: ECPrivateKey,
      message: ByteVector,
      auxRand: ByteVector): ECPrivateKey = {
    val privKeyForUse = privKey.schnorrKey

    val randHash = CryptoUtil.sha256SchnorrAuxRand(auxRand).bytes
    val maskedKey = randHash.xor(privKeyForUse.bytes)

    val nonceHash = CryptoUtil.sha256SchnorrNonce(
      maskedKey ++ privKey.schnorrPublicKey.bytes ++ message)

    ECPrivateKey(nonceHash.bytes).nonceKey
  }

  /** Computes the bip-schnorr nonce for a given message and private key.
    * This is intended to ensure that no two messages are signed with the
    * same nonce.
    */
  def fromBipSchnorr(
      privKey: ECPrivateKey,
      message: ByteVector,
      auxRand: ByteVector): SchnorrNonce = {
    val k = kFromBipSchnorr(privKey, message, auxRand)
    k.publicKey.schnorrNonce
  }

  def apply(xCoor: FieldElement): SchnorrNonce = {
    SchnorrNonce(xCoor.bytes)
  }
}
