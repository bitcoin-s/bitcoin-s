package org.bitcoins.core.util

import java.math.BigInteger
import java.security.MessageDigest

import org.bitcoins.core.crypto._
import org.bouncycastle.crypto.digests.{RIPEMD160Digest, SHA512Digest}
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import org.bouncycastle.math.ec.ECPoint
import scodec.bits.{BitVector, ByteVector}

/**
  * Created by chris on 1/14/16.
  * Utility cryptographic functions
  */
trait CryptoUtil extends BitcoinSLogger {

  /** Does the following computation: RIPEMD160(SHA256(hex)).*/
  def sha256Hash160(hex: String): Sha256Hash160Digest =
    sha256Hash160(BitcoinSUtil.decodeHex(hex))

  def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val hash = ripeMd160(sha256(bytes).bytes).bytes
    Sha256Hash160Digest(hash)
  }

  /** Performs sha256(sha256(hex)). */
  def doubleSHA256(hex: String): DoubleSha256Digest =
    doubleSHA256(BitcoinSUtil.decodeHex(hex))

  /** Performs sha256(sha256(bytes)). */
  def doubleSHA256(bytes: ByteVector): DoubleSha256Digest = {
    val hash: ByteVector = sha256(sha256(bytes).bytes).bytes
    DoubleSha256Digest(hash)
  }

  /** Takes sha256(hex). */
  def sha256(hex: String): Sha256Digest = sha256(BitcoinSUtil.decodeHex(hex))

  /** Takes sha256(bytes). */
  def sha256(bytes: ByteVector): Sha256Digest = {
    val hash = MessageDigest.getInstance("SHA-256").digest(bytes.toArray)
    Sha256Digest(ByteVector(hash))
  }

  /** Takes sha256(bits). */
  def sha256(bits: BitVector): Sha256Digest = {
    sha256(bits.toByteVector)
  }

  /** Performs SHA1(bytes). */
  def sha1(bytes: ByteVector): Sha1Digest = {
    val hash = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList
    Sha1Digest(ByteVector(hash))
  }

  /** Performs SHA1(hex). */
  def sha1(hex: String): Sha1Digest = sha1(BitcoinSUtil.decodeHex(hex))

  /** Performs RIPEMD160(hex). */
  def ripeMd160(hex: String): RipeMd160Digest =
    ripeMd160(BitcoinSUtil.decodeHex(hex))

  /** Performs RIPEMD160(bytes). */
  def ripeMd160(bytes: ByteVector): RipeMd160Digest = {
    //from this tutorial http://rosettacode.org/wiki/RIPEMD-160#Scala
    val messageDigest = new RIPEMD160Digest
    val raw = bytes.toArray
    messageDigest.update(raw, 0, raw.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize())(0)
    messageDigest.doFinal(out, 0)
    RipeMd160Digest(ByteVector(out))
  }

  val emptyDoubleSha256Hash = DoubleSha256Digest(
    "0000000000000000000000000000000000000000000000000000000000000000")

  /**
    * Calculates `HMAC-SHA512(key, data)`
    */
  def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val hmac512 = new HMac(new SHA512Digest())
    hmac512.init(new KeyParameter(key.toArray))
    hmac512.update(data.toArray, 0, data.intSize.get)
    val output = new Array[Byte](64)
    hmac512.doFinal(output, 0)
    ByteVector(output)
  }

  /**
    *
    * @param x x coordinate
    * @return a tuple (p1, p2) where p1 and p2 are points on the curve and p1.x = p2.x = x
    *         p1.y is even, p2.y is odd
    */
  def recoverPoint(x: BigInteger): (ECPoint, ECPoint) = {
    val curve = CryptoParams.curve.getCurve()
    val x1 = curve.fromBigInteger(x)
    val square = x1.square().add(curve.getA).multiply(x1).add(curve.getB)
    val y1 = square.sqrt()
    val y2 = y1.negate()
    val R1 = curve.createPoint(x1.toBigInteger, y1.toBigInteger).normalize()
    val R2 = curve.createPoint(x1.toBigInteger, y2.toBigInteger).normalize()
    if (y1.testBitZero()) (R2, R1) else (R1, R2)
  }

  /**
    * Recover public keys from a signature and the message that was signed. This method will return 2 public keys, and the signature
    * can be verified with both, but only one of them matches that private key that was used to generate the signature.
    *
    * @param signature       signature
    * @param message message that was signed
    * @return a (pub1, pub2) tuple where pub1 and pub2 are candidates public keys. If you have the recovery id  then use
    *         pub1 if the recovery id is even and pub2 if it is odd
    */
  def recoverPublicKey(
      signature: ECDigitalSignature,
      message: ByteVector): (ECPublicKey, ECPublicKey) = {

    val curve = CryptoParams.curve
    val (r, s) = (signature.r.bigInteger, signature.s.bigInteger)

    val m = new BigInteger(1, message.toArray)

    val (p1, p2) = recoverPoint(r)

    val Q1 = (p1
      .multiply(s)
      .subtract(curve.getG.multiply(m)))
      .multiply(r.modInverse(curve.getN))
    val Q2 = (p2
      .multiply(s)
      .subtract(curve.getG.multiply(m)))
      .multiply(r.modInverse(curve.getN))

    val pub1 = ECPublicKey.fromPoint(Q1)
    val pub2 = ECPublicKey.fromPoint(Q2)
    (pub1, pub2)
  }
}

object CryptoUtil extends CryptoUtil
