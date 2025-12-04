package org.bitcoins.core.bip47

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
    val tweak = FieldElement(privKey.bytes)
    val resultPoint = pubKey.multiply(tweak)
    // The result is the x-coordinate of the shared point (32 bytes)
    // For compressed public keys, skip the prefix byte and take 32 bytes
    val secret = resultPoint.bytes.tail.take(32)
    SecretPointImpl(secret)
  }
}
