package org.bitcoins.core.util

import java.security.MessageDigest

import org.bitcoins.core.crypto._
import org.bouncycastle.crypto.digests.{ RIPEMD160Digest, SHA512Digest }
import org.bouncycastle.crypto.macs.HMac
import org.bouncycastle.crypto.params.KeyParameter
import scodec.bits.ByteVector

/**
 * Created by chris on 1/14/16.
 * Utility cryptographic functions
 */
trait CryptoUtil {

  /** Does the following computation: RIPEMD160(SHA256(hex)).*/
  def sha256Hash160(hex: String): Sha256Hash160Digest = sha256Hash160(BitcoinSUtil.decodeHex(hex))

  def sha256Hash160(bytes: ByteVector): Sha256Hash160Digest = {
    val hash = ripeMd160(sha256(bytes).bytes).bytes
    Sha256Hash160Digest(hash)
  }

  /** Performs sha256(sha256(hex)). */
  def doubleSHA256(hex: String): DoubleSha256Digest = doubleSHA256(BitcoinSUtil.decodeHex(hex))

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

  /** Performs SHA1(bytes). */
  def sha1(bytes: ByteVector): Sha1Digest = {
    val hash = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList
    Sha1Digest(ByteVector(hash))
  }

  /** Performs SHA1(hex). */
  def sha1(hex: String): Sha1Digest = sha1(BitcoinSUtil.decodeHex(hex))

  /** Performs RIPEMD160(hex). */
  def ripeMd160(hex: String): RipeMd160Digest = ripeMd160(BitcoinSUtil.decodeHex(hex))

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

  val emptyDoubleSha256Hash = DoubleSha256Digest("0000000000000000000000000000000000000000000000000000000000000000")

  def hmac512(key: ByteVector, data: ByteVector): ByteVector = {
    val hmac512 = new HMac(new SHA512Digest())
    hmac512.init(new KeyParameter(key.toArray))
    hmac512.update(data.toArray, 0, data.intSize.get)
    val output = new Array[Byte](64)
    hmac512.doFinal(output, 0)
    ByteVector(output)
  }
}

object CryptoUtil extends CryptoUtil
