package org.bitcoins.core.util

import java.security.MessageDigest

import org.spongycastle.crypto.digests.RIPEMD160Digest

/**
 * Created by chris on 1/14/16.
 * Utility cryptographic functions
 */
trait CryptoUtil {

  /**
   * Does the following computation
   * RIPEMD160(SHA256(hex))
   * @param hex
   * @return
   */
  def sha256Hash160(hex : String) : Seq[Byte] = sha256Hash160(BitcoinSUtil.decodeHex(hex))


  def sha256Hash160(bytes : Seq[Byte]) = {
    val hash = ripeMd160(sha256(bytes.toArray))
    hash.toList
  }

  /**
   * Performs sha256(sha256(hex))
   * @param hex
   * @return
   */
  def doubleSHA256(hex : String) : Seq[Byte] = doubleSHA256(BitcoinSUtil.decodeHex(hex))


  /**
   * Performs sha256(sha256(bytes))
   * @param bytes
   * @return
   */
  def doubleSHA256(bytes : Seq[Byte]) : Seq[Byte] = {
    val hash : Seq[Byte] = sha256(sha256(bytes))
    hash
  }

  /**
   * Takes sha256(hex)
   * @param hex
   * @return
   */
  def sha256(hex : String) : Seq[Byte] = sha256(BitcoinSUtil.decodeHex(hex))

  /**
   * Takes sha256(bytes)
   * @param bytes
   * @return
   */
  def sha256(bytes : Seq[Byte]) : Seq[Byte] = MessageDigest.getInstance("SHA-256").digest(bytes.toArray).toList


  /**
   * Performs SHA1(bytes)
   * @param bytes
   * @return
   */
  def sha1(bytes : Seq[Byte]) : Seq[Byte] = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList



  /**
   * Performs SHA1(hex)
   * @param hex
   * @return
   */
  def sha1(hex : String) : Seq[Byte] = sha1(BitcoinSUtil.decodeHex(hex))


  /**
   * Performs RIPEMD160(hex)
   * @param hex
   * @return
   */
  def ripeMd160(hex : String) : Seq[Byte] = ripeMd160(BitcoinSUtil.decodeHex(hex))


  /**
   * Performs RIPEMD160(bytes)
   * @param bytes
   * @return
   */
  def ripeMd160(bytes : Seq[Byte]) : Seq[Byte] = {
    //from this tutorial http://rosettacode.org/wiki/RIPEMD-160#Scala
    val messageDigest = new RIPEMD160Digest
    val raw = bytes.toArray
    messageDigest.update(raw, 0, raw.length)
    val out = Array.fill[Byte](messageDigest.getDigestSize())(0)
    messageDigest.doFinal(out, 0)
    out.toList
  }


}

object CryptoUtil extends CryptoUtil
