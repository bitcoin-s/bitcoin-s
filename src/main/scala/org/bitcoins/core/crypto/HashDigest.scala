package org.bitcoins.core.crypto

import org.bitcoins.core.util.BitcoinSUtil

/**
  * Created by chris on 5/24/16.
  */
sealed trait HashDigest {
  /**
    * The message digest represented in bytes
    * @return
    */
  def bytes : Seq[Byte]

  /**
    * The message digest represented in hexadecimal
    * @return
    */
  def hex : String = BitcoinSUtil.encodeHex(bytes)
}


/**
  * Represents the result of SHA1()
  * @param bytes
  */
case class Sha1Digest(bytes : Seq[Byte]) extends HashDigest

/**
  * Represents the result of SHA256()
  * @param bytes
  */
case class Sha256Digest(bytes : Seq[Byte]) extends HashDigest

/**
  * Represents the result of SHA256(SHA256())
  * @param bytes
  */
case class DoubleSha256Digest(bytes : Seq[Byte]) extends HashDigest

/**
  * Represents the result of RIPEMD160()
  * @param bytes
  */
case class RipeMd160Digest(bytes : Seq[Byte]) extends HashDigest

/**
  * Represents the result of RIPEMD160(SHA256())
  * @param bytes
  */
case class Sha256Hash160Digest(bytes : Seq[Byte]) extends HashDigest