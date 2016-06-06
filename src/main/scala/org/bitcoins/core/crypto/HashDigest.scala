package org.bitcoins.core.crypto

import org.bitcoins.core.util.{Factory, BitcoinSUtil}

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
  */
sealed trait DoubleSha256Digest extends HashDigest

object DoubleSha256Digest extends Factory[DoubleSha256Digest] {
  private case class DoubleSha256DigestImpl(bytes: Seq[Byte]) extends DoubleSha256Digest {
    override def toString = "DoubleSha256DigestImpl(" + hex + ")"
  }
  override def fromBytes(bytes : Seq[Byte]) : DoubleSha256Digest = DoubleSha256DigestImpl(bytes)

}

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