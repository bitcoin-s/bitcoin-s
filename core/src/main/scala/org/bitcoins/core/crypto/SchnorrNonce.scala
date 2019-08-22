package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{BitcoinSUtil, CryptoUtil}
import scodec.bits.ByteVector

import scala.annotation.tailrec

case class SchnorrNonce(bytes: ByteVector) extends NetworkElement {
  require(bytes.length == 32, s"Schnorr nonce must be 32 bytes, got $bytes")

  def publicKey: ECPublicKey = Schnorr.computeR(this)
}

object SchnorrNonce {

  @tailrec
  def fromBytes(bytes: ByteVector): SchnorrNonce = {
    require(bytes.length <= 33,
            s"Schnorr Nonce must be less than 33 bytes, got $bytes")

    if (bytes.length == 32)
      SchnorrNonce(bytes)
    else if (bytes.length < 32) {
      // means we need to pad the private key with 0 bytes so we have 32 bytes
      SchnorrNonce.fromBytes(bytes.padLeft(32))
    } else if (bytes.length == 33) {
      // this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
      SchnorrNonce.fromBytes(bytes.tail)
    } else {
      throw new IllegalArgumentException(
        "Scnorr nonce cannot be greater than 33 bytes in size, got: " +
          BitcoinSUtil.encodeHex(bytes) + " which is of size: " + bytes.size)
    }
  }

  /** Computes the bip-schnorr nonce for a given message and private key.
    * This is intended to ensure that no two messages are signed with the
    * same nonce.
    * @see [[https://github.com/sipa/bips/blob/bip-schnorr/bip-schnorr.mediawiki]]
    */
  def fromBipSchnorr(
      privateKey: ECPrivateKey,
      message: ByteVector): SchnorrNonce = {
    require(message.length == 32, s"Message must be 32 bytes, got $message")

    val nonceBytes = CryptoUtil.sha256(privateKey.bytes ++ message).bytes
    SchnorrNonce.fromBytes(nonceBytes)
  }
}
