package org.bitcoins.core.crypto

import org.bitcoin.{NativeSecp256k1, Secp256k1Context}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{BitcoinSUtil, Factory}
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.Try

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
          BitcoinSUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
    }
  }

  def apply(xCoor: FieldElement): SchnorrPublicKey = {
    SchnorrPublicKey(xCoor.bytes)
  }
}
