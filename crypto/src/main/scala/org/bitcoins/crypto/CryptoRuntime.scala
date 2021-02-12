package org.bitcoins.crypto

import scodec.bits.ByteVector

/** Trait that should be extended by specific runtimes like javascript
  * or the JVM to support crypto functions needed for bitcoin-s
  */
trait CryptoRuntime {

  /** Generates a 32 byte private key */
  def freshPrivateKey: ECPrivateKey

  /** Converts a private key -> public key
    * @param privateKey the private key we want the corresponding public key for
    * @param isCompressed whether the returned public key should be compressed or not
    */
  def toPublicKey(privateKey: ECPrivateKey, isCompressed: Boolean): ECPublicKey

  def ripeMd160(bytes: ByteVector): RipeMd160Digest

  def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest

  def sha256(bytes: ByteVector): Sha256Digest

  /** Performs sha256(sha256(bytes)). */
  def doubleSHA256(bytes: ByteVector): DoubleSha256Digest = {
    val hash: ByteVector = sha256(sha256(bytes).bytes).bytes
    DoubleSha256Digest(hash)
  }
}
