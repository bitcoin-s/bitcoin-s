package org.bitcoins.core.crypto

import org.bitcoin.NativeSecp256k1
import scodec.bits.ByteVector

object Schnorr {

  def sign(
      msg: ByteVector,
      privateKey: ECPrivateKey): SchnorrDigitalSignature = {
    if (msg.length != 32) {
      throw new IllegalArgumentException(s"Message must be 32 bytes, got $msg")
    } else {
      val sigArr =
        NativeSecp256k1.schnorrSign(msg.toArray, privateKey.bytes.toArray)

      SchnorrDigitalSignature.fromBytes(ByteVector(sigArr))
    }
  }

  def signWithNonce(
      msg: ByteVector,
      privateKey: ECPrivateKey,
      nonce: ECPrivateKey): SchnorrDigitalSignature = {
    if (msg.length != 32) {
      throw new IllegalArgumentException(s"Message must be 32 bytes, got $msg")
    } else {
      val sigArr =
        NativeSecp256k1.schnorrSignWithNonce(msg.toArray,
                                             privateKey.bytes.toArray,
                                             nonce.bytes.toArray)

      SchnorrDigitalSignature.fromBytes(ByteVector(sigArr))
    }
  }

  def verify(
      msg: ByteVector,
      sig: SchnorrDigitalSignature,
      publicKey: ECPublicKey): Boolean = {
    if (msg.length != 32) {
      throw new IllegalArgumentException(s"Message must be 32 bytes, got $msg")
    } else {
      NativeSecp256k1.schnorrVerify(msg.toArray,
                                    sig.bytes.toArray,
                                    publicKey.bytes.toArray)
    }
  }

  def computePubKey(
      msg: ByteVector,
      r: ECPublicKey,
      publicKey: ECPublicKey): ECPublicKey = {
    if (msg.length != 32) {
      throw new IllegalArgumentException(s"Message must be 32 bytes, got $msg")
    } else {
      val sigKeyArr = NativeSecp256k1.computeSchnorrPubKey(
        msg.toArray,
        r.bytes.toArray,
        publicKey.bytes.toArray)

      ECPublicKey.fromBytes(ByteVector(sigKeyArr))
    }
  }

  def computeR(nonce: ECPrivateKey): ECPublicKey = {
    val keyArr = NativeSecp256k1.schnorrPublicNonce(nonce.bytes.toArray)

    ECPublicKey.fromBytes(ByteVector(keyArr))
  }
}
