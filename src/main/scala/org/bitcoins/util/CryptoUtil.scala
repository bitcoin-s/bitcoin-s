package org.bitcoins.util

import java.security.MessageDigest

import org.bitcoinj.core.Sha256Hash
import org.bitcoins.script.constant.{ScriptConstantImpl, ScriptConstant}
import org.spongycastle.crypto.digests.RIPEMD160Digest

/**
 * Created by chris on 1/14/16.
 * Utility cryptographic functions
 */
trait CryptoUtil {

  /**
   * Does the following computation
   * RIPEMD160(SHA256(hex))
 *
   * @param hex
   * @return
   */
  def sha256Hash160(hex : String) : List[Byte] = sha256Hash160(BitcoinSUtil.decodeHex(hex))

  def sha256Hash160(bytes : List[Byte]) = {
    val hash = org.bitcoinj.core.Utils.sha256hash160(bytes.toArray)
    hash.toList
  }
  /**
   * Performs sha256(sha256(hex))
 *
   * @param hex
   * @return
   */
  def doubleSHA256(hex : String) : List[Byte] = doubleSHA256(BitcoinSUtil.decodeHex(hex))
  /**
   * Performs sha256(sha256(hex))
 *
   * @param bytes
   * @return
   */
  def doubleSHA256(bytes : List[Byte]) : List[Byte] = doubleSHA256(bytes.toSeq)

  /**
   * Performs sha256(sha256(bytes))
 *
   * @param bytes
   * @return
   */
  def doubleSHA256(bytes : Seq[Byte]) : List[Byte] = {
    val hash : List[Byte] = Sha256Hash.hashTwice(bytes.toArray).toList
    hash
  }

  /**
   * Takes sha256(hex)
 *
   * @param hex
   * @return
   */
  def sha256(hex : String) : List[Byte] = sha256(BitcoinSUtil.decodeHex(hex))

  /**
   * Takes sha256(bytes)
 *
   * @param bytes
   * @return
   */
  def sha256(bytes : List[Byte]) : List[Byte] = {
    val hash : List[Byte] = Sha256Hash.hash(bytes.toArray).toList
    hash
  }

  /**
   * Performs SHA1(bytes)
 *
   * @param bytes
   * @return
   */
  def sha1(bytes : List[Byte]) : List[Byte] = MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toList



  /**
   * Performs SHA1(hex)
 *
   * @param hex
   * @return
   */
  def sha1(hex : String) : List[Byte] = sha1(BitcoinSUtil.decodeHex(hex))


  /**
   * Performs RIPEMD160(hex)
 *
   * @param hex
   * @return
   */
  def ripeMd160(hex : String) : List[Byte] = ripeMd160(BitcoinSUtil.decodeHex(hex))


  /**
   * Performs RIPEMD160(bytes)
 *
   * @param bytes
   * @return
   */
  def ripeMd160(bytes : List[Byte]) : List[Byte] = {
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
