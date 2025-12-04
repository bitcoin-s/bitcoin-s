package org.bitcoins.core.bip47

import org.bitcoin.{NativeSecp256k1, Secp256k1Context}
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey, FieldElement}
import scodec.bits.ByteVector

/** Represents an ECDH shared secret point between two parties. Used in BIP47 to
  * derive shared secrets for payment address generation.
  */
sealed abstract class SecretPoint {
  def ecdhSecret: ByteVector
}

object SecretPoint {

  private case class SecretPointImpl(ecdhSecret: ByteVector)
      extends SecretPoint {
    require(ecdhSecret.size == 32,
            s"ECDH secret must be 32 bytes, got ${ecdhSecret.size}")
  }

  /** Computes the ECDH shared secret between a private key and public key. The
    * result is the x-coordinate of privKey * pubKey.
    */
  def apply(privKey: ECPrivateKey, pubKey: ECPublicKey): SecretPoint = {
    val secret =
      if (Secp256k1Context.isEnabled) {
        computeECDHNative(privKey, pubKey)
      } else {
        computeECDHPure(privKey, pubKey)
      }
    SecretPointImpl(secret)
  }

  private def computeECDHNative(
      privKey: ECPrivateKey,
      pubKey: ECPublicKey): ByteVector = {
    val result = NativeSecp256k1.createECDHSecret(privKey.bytes.toArray,
                                                  pubKey.bytes.toArray)
    ByteVector(result)
  }

  private def computeECDHPure(
      privKey: ECPrivateKey,
      pubKey: ECPublicKey): ByteVector = {
    val tweak = FieldElement(privKey.bytes)
    val resultPoint = pubKey.multiply(tweak)
    resultPoint.bytes.tail.take(32)
  }
}
