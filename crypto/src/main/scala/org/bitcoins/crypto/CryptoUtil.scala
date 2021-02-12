package org.bitcoins.crypto

import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector

import java.math.BigInteger

/** Utility cryptographic functions
  * This is a proxy for the underlying implementation of [[CryptoRuntime]]
  * such as [[JvmCryptoRuntime]].
  *
  * This is necessary so that the core module doesn't need to be refactored
  * to add support for multiple platforms, it can keep referencing CryptoUtil
  */
trait CryptoUtil extends CryptoRuntime {

  /** The underlying runtime for the specific platform we are running on */
  private lazy val cryptoRuntime: CryptoRuntime = CryptoContext.cryptoRuntime

  override def freshPrivateKey: ECPrivateKey = {
    cryptoRuntime.freshPrivateKey
  }

  override def toPublicKey(
      privateKey: ECPrivateKey,
      isCompressed: Boolean): ECPublicKey = {
    cryptoRuntime.toPublicKey(privateKey, isCompressed)
  }

  override def normalize(str: String): String = {
    cryptoRuntime.normalize(str)
  }

  /** Does the following computation: RIPEMD160(SHA256(hex)). */
  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    cryptoRuntime.sha256Hash160(bytes)
  }

  def sha256Hash160(str: String): Sha256Hash160Digest = {
    cryptoRuntime.sha256Hash160(serializeForHash(str))
  }

  def doubleSHA256(str: String): DoubleSha256Digest = {
    doubleSHA256(serializeForHash(str))
  }

  /** Takes sha256(bytes). */
  override def sha256(bytes: ByteVector): Sha256Digest = {
    cryptoRuntime.sha256(bytes)
  }

  def taggedSha256(str: String, tag: String): Sha256Digest = {
    taggedSha256(serializeForHash(str), tag)
  }

  /** Performs SHA1(bytes). */
  override def sha1(bytes: ByteVector): Sha1Digest = {
    cryptoRuntime.sha1(bytes)
  }

  def sha1(str: String): Sha1Digest = {
    sha1(serializeForHash(str))
  }

  /** Performs RIPEMD160(bytes). */
  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    cryptoRuntime.ripeMd160(bytes)
  }

  def ripeMd160(str: String): RipeMd160Digest = {
    ripeMd160(serializeForHash(str))
  }

  /** Calculates `HMAC-SHA512(key, data)`
    */
  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    cryptoRuntime.hmac512(key, data)
  }

  /** @param x x coordinate
    * @return a tuple (p1, p2) where p1 and p2 are points on the curve and p1.x = p2.x = x
    *         p1.y is even, p2.y is odd
    */
  def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = {
    cryptoRuntime.recoverPoint(x)
  }

  override def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) = {
    cryptoRuntime.recoverPublicKey(signature, message)
  }
}

object CryptoUtil extends CryptoUtil
