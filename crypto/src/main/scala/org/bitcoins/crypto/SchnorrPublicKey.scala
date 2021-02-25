package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.Try

case class SchnorrPublicKey(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 32,
          s"Schnorr public keys must be 32 bytes, got $bytes")
  require(Try(publicKey).isSuccess,
          s"Schnorr public key must be a valid x coordinate, got $bytes")

  def verify(data: ByteVector, signature: SchnorrDigitalSignature): Boolean = {
    CryptoUtil.schnorrVerify(data, this, signature)
  }

  def computeSigPoint(data: ByteVector, nonce: SchnorrNonce): ECPublicKey = {
    computeSigPoint(data, nonce, compressed = true)
  }

  def computeSigPoint(hash: HashDigest, nonce: SchnorrNonce): ECPublicKey = {
    computeSigPoint(hash.bytes, nonce)
  }

  def computeSigPoint(
      bytesToHash: Vector[ByteVector],
      nonces: Vector[SchnorrNonce]): ECPublicKey = {
    // TODO: when combine function is ported from secp, use that instead for nonces
    val bytesAndNonces = bytesToHash.zip(nonces)

    val hashesAndNoncePoints = bytesAndNonces.map { case (bytes, nonce) =>
      val eBytes = CryptoUtil
        .sha256SchnorrChallenge(
          nonce.bytes ++ this.bytes ++ CryptoUtil
            .sha256DLCAttestation(bytes)
            .bytes)
        .bytes
      val e = ECPrivateKey(eBytes)
      (e, nonce.publicKey)
    }

    val (aggHashes, aggNonces) =
      hashesAndNoncePoints.reduce[(ECPrivateKey, ECPublicKey)] {
        case ((aggHash, aggPoint), (hash, nonce)) =>
          (aggHash.add(hash), aggPoint.add(nonce))
      }

    this.publicKey.tweakMultiply(aggHashes.fieldElement).add(aggNonces)
  }

  def computeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      compressed: Boolean): ECPublicKey = {
    CryptoUtil.schnorrComputeSigPoint(data, nonce, this, compressed)
  }

  def publicKey: ECPublicKey = {
    val pubKeyBytes = ByteVector.fromByte(2) ++ bytes

    val validPubKey = CryptoUtil.isValidPubKey(pubKeyBytes)

    require(
      validPubKey,
      s"Cannot construct schnorr public key from invalid x coordinate: $bytes")

    ECPublicKey(pubKeyBytes)
  }

  def xCoord: FieldElement = FieldElement(bytes)
}

object SchnorrPublicKey extends Factory[SchnorrPublicKey] {

  @tailrec
  def fromBytes(bytes: ByteVector): SchnorrPublicKey = {
    require(bytes.length <= 33,
            s"XOnlyPublicKey must be less than 33 bytes, got $bytes")

    if (bytes.length == 32)
      new SchnorrPublicKey(bytes)
    else if (bytes.length < 32) {
      // means we need to pad the private key with 0 bytes so we have 32 bytes
      SchnorrPublicKey.fromBytes(bytes.padLeft(32))
    } else if (bytes.length == 33) {
      // this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
      SchnorrPublicKey.fromBytes(bytes.tail)
    } else {
      throw new IllegalArgumentException(
        "XOnlyPublicKey cannot be greater than 33 bytes in size, got: " +
          CryptoBytesUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
    }
  }

  def apply(xCoor: FieldElement): SchnorrPublicKey = {
    SchnorrPublicKey(xCoor.bytes)
  }
}
