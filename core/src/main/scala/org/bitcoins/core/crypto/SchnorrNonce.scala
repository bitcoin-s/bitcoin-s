package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{CryptoUtil, Factory}
import scodec.bits.ByteVector

import scala.annotation.tailrec

sealed abstract class SchnorrNonce extends NetworkElement {
  def publicKey: ECPublicKey = Schnorr.computeR(this)
}

object SchnorrNonce extends Factory[SchnorrNonce] {
  private case class SchnorrNonceImpl(bytes: ByteVector) extends SchnorrNonce {
    override def toString: String = {
      s"SchnorrNonce($bytes)"
    }
  }

  @tailrec
  override def fromBytes(bytes: ByteVector): SchnorrNonce = {
    require(bytes.length <= 33, s"Schnorr Nonce must be 32 bytes, got $bytes")

    if (bytes.size == 32)
      SchnorrNonceImpl(bytes)
    else if (bytes.size < 32) {
      //means we need to pad the private key with 0 bytes so we have 32 bytes
      SchnorrNonce.fromBytes(bytes.padLeft(32))
    } //this is for the case when java serialies a BigInteger to 33 bytes to hold the signed num representation
    else {
      SchnorrNonce.fromBytes(bytes.tail)
    }
  }

  def fromBipSchnorr(
      privateKey: ECPrivateKey,
      message: ByteVector): SchnorrNonce = {
    require(message.length == 32, s"Message must be 32 bytes, got $message")

    val nonceBytes = CryptoUtil.sha256(privateKey.bytes ++ message).bytes
    fromBytes(nonceBytes)
  }
}
