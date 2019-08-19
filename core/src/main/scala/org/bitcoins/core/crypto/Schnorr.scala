package org.bitcoins.core.crypto

import org.bitcoin.NativeSecp256k1
import scodec.bits.ByteVector

object Schnorr {

  def sign(
      msg: ByteVector,
      privateKey: ECPrivateKey): SchnorrDigitalSignature = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    val sigArr =
      NativeSecp256k1.schnorrSign(msg.toArray, privateKey.bytes.toArray)

    SchnorrDigitalSignature.fromBytes(ByteVector(sigArr))
  }

  def signWithNonce(
      msg: ByteVector,
      privateKey: ECPrivateKey,
      nonce: SchnorrNonce): SchnorrDigitalSignature = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    val sigArr =
      NativeSecp256k1.schnorrSignWithNonce(msg.toArray,
                                           privateKey.bytes.toArray,
                                           nonce.bytes.toArray)

    SchnorrDigitalSignature.fromBytes(ByteVector(sigArr))
  }

  def verify(
      msg: ByteVector,
      sig: SchnorrDigitalSignature,
      publicKey: ECPublicKey): Boolean = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    NativeSecp256k1.schnorrVerify(msg.toArray,
                                  sig.bytes.toArray,
                                  publicKey.bytes.toArray)
  }

  def computePubKey(
      msg: ByteVector,
      r: ECPublicKey,
      publicKey: ECPublicKey): ECPublicKey = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    val sigKeyArr = NativeSecp256k1.computeSchnorrPubKey(
      msg.toArray,
      r.bytes.toArray,
      publicKey.bytes.toArray)

    ECPublicKey.fromBytes(ByteVector(sigKeyArr))
  }

  def computeR(nonce: SchnorrNonce): ECPublicKey = {
    val keyArr = NativeSecp256k1.schnorrPublicNonce(nonce.bytes.toArray)

    ECPublicKey.fromBytes(ByteVector(keyArr))
  }
}
