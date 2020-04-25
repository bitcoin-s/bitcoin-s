package org.bitcoins.crypto

import org.bitcoin.{NativeSecp256k1, Secp256k1Context}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.Try

/**
 * This represents the x-coordinate for the point on the curve.
 * With BIP340 bitcoin is moving to only explicitly encode x coordinate on the curve for savings in
 * blockchain space and optimizations like batch verification. This means that we only
 * need this public key encoding to use 32 bytes rather than 33 or 65 bytes traditional bitcoin public keys take.
 *
 * From BIP340:
 * SchnorrPublicKey represents the X coordinate of a point P on the curve whose Y coordinate is a square and signatures (r,s) where r
 * is the X coordinate of a point R whose Y coordinate is a square. The signature satisfies s⋅G = R + tagged_hash(r || pk || m)⋅P.
 *
 * You can retrieve the full public key by calling [[SchnorrPublicKey.publicKey]]
 * @see https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki#design
 * @see https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki#cite_ref-5-0*/
case class SchnorrPublicKey(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 32,
          s"Schnorr public keys must be 32 bytes, got $bytes")
  require(Try(publicKey).isSuccess,
          s"Schnorr public key must be a valid x coordinate, got $bytes")

  def verify(data: ByteVector, signature: SchnorrDigitalSignature): Boolean = {
    verify(data, signature, Secp256k1Context.isEnabled)
  }

  def verify(
      data: ByteVector,
      signature: SchnorrDigitalSignature,
      useSecp: Boolean): Boolean = {
    if (useSecp) {
      verifyWithSecp(data, signature)
    } else {
      verifyWithBouncyCastle(data, signature)
    }
  }

  def verifyWithSecp(
      data: ByteVector,
      signature: SchnorrDigitalSignature): Boolean = {
    NativeSecp256k1.schnorrVerify(signature.bytes.toArray,
                                  data.toArray,
                                  bytes.toArray)
  }

  def verifyWithBouncyCastle(
      data: ByteVector,
      signature: SchnorrDigitalSignature): Boolean = {
    BouncyCastleUtil.schnorrVerify(data, this, signature)
  }

  def computeSigPoint(data: ByteVector, nonce: SchnorrNonce): ECPublicKey = {
    computeSigPoint(data, nonce, compressed = true, Secp256k1Context.isEnabled)
  }

  def computeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      compressed: Boolean): ECPublicKey = {
    computeSigPoint(data, nonce, compressed, Secp256k1Context.isEnabled)
  }

  def computeSigPoint(
      data: ByteVector,
      nonce: SchnorrNonce,
      compressed: Boolean,
      useSecp: Boolean): ECPublicKey = {
    if (useSecp) {
      computeSigPointWithSecp(data, nonce, compressed)
    } else {
      computeSigPointWithBouncyCastle(data, nonce, compressed)
    }
  }

  def computeSigPointWithSecp(
      data: ByteVector,
      nonce: SchnorrNonce,
      compressed: Boolean = true): ECPublicKey = {
    val sigPointBytes = NativeSecp256k1.schnorrComputeSigPoint(
      data.toArray,
      nonce.bytes.toArray,
      bytes.toArray,
      compressed)
    ECPublicKey(ByteVector(sigPointBytes))
  }

  def computeSigPointWithBouncyCastle(
      data: ByteVector,
      nonce: SchnorrNonce,
      compressed: Boolean = true): ECPublicKey = {
    BouncyCastleUtil.schnorrComputeSigPoint(data, nonce, this, compressed)
  }

  def publicKey: ECPublicKey = {
    val pubKeyBytes = ByteVector.fromByte(2) ++ bytes

    val validPubKey = if (Secp256k1Context.isEnabled) {
      NativeSecp256k1.isValidPubKey(pubKeyBytes.toArray)
    } else {
      BouncyCastleUtil.validatePublicKey(pubKeyBytes)
    }

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
          BytesUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
    }
  }

  def apply(xCoor: FieldElement): SchnorrPublicKey = {
    SchnorrPublicKey(xCoor.bytes)
  }
}
