package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.util.Try

case class SchnorrPublicKey(bytes: ByteVector) extends PublicKey {
  require(bytes.length == 32,
          s"Schnorr public keys must be 32 bytes, got $bytes")
  require(Try(publicKey).isSuccess,
          s"Schnorr public key must be a valid x coordinate, got $bytes")

  def verify(data: ByteVector, signature: SchnorrDigitalSignature): Boolean = {
    CryptoUtil.schnorrVerify(data, this, signature)
  }

  def verify(hash: HashDigest, signature: SchnorrDigitalSignature): Boolean = {
    verify(hash.bytes, signature)
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

    this.publicKey.multiply(aggHashes.fieldElement).add(aggNonces)
  }

  def computeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      compressed: Boolean): ECPublicKey = {
    CryptoUtil.schnorrComputeSigPoint(data, nonce, this, compressed)
  }

  lazy val publicKey: ECPublicKey = {
    val pubKeyBytes = EvenParity.bytes ++ bytes

    ECPublicKey(pubKeyBytes)
  }

  def xCoord: CurveCoordinate = CurveCoordinate(bytes)

  def toXOnly: XOnlyPubKey = XOnlyPubKey(bytes)

  override def toString: String = s"SchnorrPublicKey(${bytes.toHex})"
}

object SchnorrPublicKey extends Factory[SchnorrPublicKey] {

  def fromBytes(bytes: ByteVector): SchnorrPublicKey = {
    require(
      bytes.length == 32,
      "SchnorrPublicKey must be exactly 32 bytes, got: " +
        CryptoBytesUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
    new SchnorrPublicKey(bytes)
  }

  def apply(xCoor: CurveCoordinate): SchnorrPublicKey = {
    SchnorrPublicKey(xCoor.bytes)
  }
}
