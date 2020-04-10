package org.bitcoins.core.crypto

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{BitcoinSUtil, Factory}
import scodec.bits.ByteVector

import scala.annotation.tailrec

case class SchnorrPublicKey(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 32,
          s"Schnorr public keys must be 32 bytes, got $bytes")

  def verify(data: ByteVector, signature: SchnorrDigitalSignature): Boolean = {
    NativeSecp256k1.schnorrVerify(signature.bytes.toArray,
                                  data.toArray,
                                  bytes.toArray)
  }

  def computeSigPoint(
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

  def publicKey: ECPublicKey = {
    ECPublicKey(s"02$hex")
  }
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
}
