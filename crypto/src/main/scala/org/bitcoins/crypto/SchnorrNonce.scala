package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.Try

case class SchnorrNonce(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 32, s"Schnorr nonce must be 32 bytes, get $bytes")

  private val evenKey: ECPublicKey = ECPublicKey(s"02$hex")
  private val oddKey: ECPublicKey = ECPublicKey(s"03$hex")

  private val yCoordEven: Boolean = {
    evenKey.toPoint.getRawYCoord.sqrt() != null
  }

  /** Computes the public key associated with a SchnorrNonce as specified in bip-schnorr.
    * They y-coordinate is chosen to be a quadratic residue.
    */
  val publicKey: ECPublicKey = {
    if (yCoordEven) {
      evenKey
    } else {
      oddKey
    }
  }

  require(Try(publicKey).isSuccess,
          s"Schnorr nonce must be a valid x coordinate, got $bytes")
  require(
    publicKey.toPoint.getRawYCoord.sqrt != null,
    "Schnorr nonce must be an x coordinate for which a quadratic residue y coordinate exists")

  def xCoord: FieldElement = {
    FieldElement(bytes)
  }
}

object SchnorrNonce extends Factory[SchnorrNonce] {

  @tailrec
  def fromBytes(bytes: ByteVector): SchnorrNonce = {
    if (bytes.length == 32) {
      new SchnorrNonce(bytes)
    } else if (bytes.length < 32) {
      // means we need to pad the private key with 0 bytes so we have 32 bytes
      SchnorrNonce.fromBytes(bytes.padLeft(32))
    } else if (bytes.length == 33) {
      // this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
      SchnorrNonce.fromBytes(bytes.tail)
    } else {
      throw new IllegalArgumentException(
        "Schnorr nonce cannot be greater than 33 bytes in size, got: " +
          CryptoBytesUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
    }
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
