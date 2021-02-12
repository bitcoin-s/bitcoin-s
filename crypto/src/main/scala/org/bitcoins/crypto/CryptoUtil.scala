package org.bitcoins.crypto

import org.bitcoin.NativeSecp256k1

import java.math.BigInteger
import java.security.{MessageDigest, SecureRandom}
import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA512Digest}
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import org.bouncycastle.math.ec.ECPoint
import scodec.bits.ByteVector

/** Utility cryptographic functions
  */
trait CryptoUtil extends CryptoRuntime {
  private[this] lazy val secureRandom = new SecureRandom()

  override def freshPrivateKey: ECPrivateKey = {
    val array = new Array[Byte](32)
    secureRandom.nextBytes(array)
    require(array.exists(_ != 0),
            s"Array did not contain sufficient entropy, got all zero bytes!")
    ECPrivateKey.fromBytes(ByteVector(array))
  }

  override def toPublicKey(
      privateKey: ECPrivateKey,
      isCompressed: Boolean): ECPublicKey = {
    CryptoContext.default match {
      case CryptoContext.BouncyCastle =>
        BouncyCastleUtil.computePublicKey(privateKey)
      case CryptoContext.LibSecp256k1 =>
        val pubKeyBytes: Array[Byte] =
          NativeSecp256k1.computePubkey(privateKey.bytes.toArray, isCompressed)
        val pubBytes = ByteVector(pubKeyBytes)
        require(
          ECPublicKey.isFullyValid(pubBytes),
          s"secp256k1 failed to generate a valid public key, got: ${CryptoBytesUtil
            .encodeHex(pubBytes)}")
        ECPublicKey(pubBytes)
    }

  }

  override def normalize(str: String): String = {
    java.text.Normalizer.normalize(str, java.text.Normalizer.Form.NFC)
  }

  /** Does the following computation: RIPEMD160(SHA256(hex)). */
  override def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val hash = ripeMd160(sha256(bytes).bytes).bytes
    Sha256Hash160Digest(hash)
  }

  def sha256Hash160(str: String): Sha256Hash160Digest = {
    sha256Hash160(serializeForHash(str))
  }

  def doubleSHA256(str: String): DoubleSha256Digest = {
    doubleSHA256(serializeForHash(str))
  }

  /** Takes sha256(bytes). */
  override def sha256(bytes: ByteVector): Sha256Digest = {
    val hash = MessageDigest.getInstance("SHA-256").digest(bytes.toArray)
    Sha256Digest(ByteVector(hash))
  }

  override def sha256(str: String): Sha256Digest = {
    sha256(serializeForHash(str))
  }

  def taggedSha256(str: String, tag: String): Sha256Digest = {
    taggedSha256(serializeForHash(str), tag)
  }

  /** Performs SHA1(bytes). */
  override def sha1(bytes: ByteVector): Sha1Digest = {
    val hash = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList
    Sha1Digest(ByteVector(hash))
  }

  def sha1(str: String): Sha1Digest = {
    sha1(serializeForHash(str))
  }

  /** Performs RIPEMD160(bytes). */
  override def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    //from this tutorial http://rosettacode.org/wiki/RIPEMD-160#Scala
    val messageDigest = new RIPEMD160Digest
    val raw = bytes.toArray
    messageDigest.update(raw, 0, raw.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize)(0)
    messageDigest.doFinal(out, 0)
    RipeMd160Digest(ByteVector(out))
  }

  def ripeMd160(str: String): RipeMd160Digest = {
    ripeMd160(serializeForHash(str))
  }

  /** Calculates `HMAC-SHA512(key, data)`
    */
  override def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val hmac512 = new HMac(new SHA512Digest())
    hmac512.init(new KeyParameter(key.toArray))
    hmac512.update(data.toArray, 0, data.intSize.get)
    val output = new Array[Byte](64)
    hmac512.doFinal(output, 0)
    ByteVector(output)
  }

  /** @param x x coordinate
    * @return a tuple (p1, p2) where p1 and p2 are points on the curve and p1.x = p2.x = x
    *         p1.y is even, p2.y is odd
    */
  def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = {
    val bytes = ByteVector(x.toByteArray)

    val bytes32 = if (bytes.length < 32) {
      bytes.padLeft(32)
    } else if (bytes.length == 32) {
      bytes
    } else if (bytes.length == 33 && bytes.head == 0.toByte) {
      bytes.tail
    } else {
      throw new IllegalArgumentException(
        s"Field element cannot have more than 32 bytes, got $bytes from $x")
    }

    (ECPublicKey(0x02.toByte +: bytes32).toPoint,
     ECPublicKey(0x03.toByte +: bytes32).toPoint)
  }

  override def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) = {

    val curve = CryptoParams.curve
    val (r, s) = (signature.r.bigInteger, signature.s.bigInteger)

    val m = new BigInteger(1, message.toArray)

    val (p1, p2) = recoverPoint(r)

    val Q1 = p1
      .multiply(s)
      .subtract(curve.getG.multiply(m))
      .multiply(r.modInverse(curve.getN))
    val Q2 = p2
      .multiply(s)
      .subtract(curve.getG.multiply(m))
      .multiply(r.modInverse(curve.getN))

    val pub1 = ECPublicKey.fromPoint(Q1)
    val pub2 = ECPublicKey.fromPoint(Q2)
    (pub1, pub2)
  }
}

object CryptoUtil extends CryptoUtil
