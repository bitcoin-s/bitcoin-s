package org.bitcoins.core.crypto

import org.bitcoin.NativeSecp256k1
import scodec.bits.ByteVector

object Schnorr {

  /** Generates a Schnorr signature for the 32 byte msg using privateKey */
  def sign(
      msg: ByteVector,
      privateKey: ECPrivateKey): SchnorrDigitalSignature = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    val sigArr =
      NativeSecp256k1.schnorrSign(msg.toArray, privateKey.bytes.toArray)

    SchnorrDigitalSignature.fromBytes(ByteVector(sigArr))
  }

  /** Generates a Schnorr signature for the 32 byte msg using privateKey
    * and nonce.
    *
    * ```IMPORTANT```: Never sign two messages with the same privateKey and nonce!
    *                  This leaks your private key publicly.
    */
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

  /** Verifies a Schnorr signature of a given msg with a given publicKey */
  def verify(
      msg: ByteVector,
      sig: SchnorrDigitalSignature,
      publicKey: ECPublicKey): Boolean = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    NativeSecp256k1.schnorrVerify(msg.toArray,
                                  sig.bytes.toArray,
                                  publicKey.bytes.toArray)
  }

  /** Computes the public key associated with Schnorr signature from public information */
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

  /** Computes the public key associated with a SchnorrNonce as specified in bip-schnorr.
    * They y-coordinate is chosen to be a quadratic residue.
    * @see [[https://github.com/sipa/bips/blob/bip-schnorr/bip-schnorr.mediawiki#design]]
    */
  def computeR(nonce: SchnorrNonce): ECPublicKey = {
    val keyArr = NativeSecp256k1.schnorrPublicNonce(nonce.bytes.toArray)

    ECPublicKey.fromBytes(ByteVector(keyArr))
  }
}
