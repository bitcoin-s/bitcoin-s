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
  */
sealed trait Sha1Digest extends HashDigest

object Sha1Digest extends Factory[Sha1Digest] {
  private case class Sha1DigestImpl(bytes : Seq[Byte]) extends Sha1Digest {
    override def toString = "Sha1DigestImpl(" + hex + ")"
  }
  override def fromBytes(bytes: Seq[Byte]) : Sha1Digest = Sha1DigestImpl(bytes)
}

/**
  * Represents the result of SHA256()
  */
sealed trait Sha256Digest extends HashDigest

object Sha256Digest extends Factory[Sha256Digest] {
  private case class Sha256DigestImpl(bytes: Seq[Byte]) extends Sha256Digest {
    override def toString = "Sha256DigestImpl(" + hex + ")"
  }
  override def fromBytes(bytes:Seq[Byte]) : Sha256Digest = Sha256DigestImpl(bytes)
}

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
  */
sealed trait RipeMd160Digest extends HashDigest

object RipeMd160Digest extends Factory[RipeMd160Digest] {
  private case class RipeMd160DigestImpl(bytes : Seq[Byte]) extends RipeMd160Digest {
    override def toString = "RipeMd160DigestImpl(" + hex + ")"
  }
  override def fromBytes(bytes : Seq[Byte]) : RipeMd160Digest = RipeMd160DigestImpl(bytes)

}
/**
  * Represents the result of RIPEMD160(SHA256())
  */
sealed trait Sha256Hash160Digest extends HashDigest

object Sha256Hash160Digest extends Factory[Sha256Hash160Digest] {
  case class Sha256Hash160DigestImpl(bytes : Seq[Byte]) extends Sha256Hash160Digest {
    override def toString = "Sha256Hash160DigestImpl(" + hex + ")"
  }
  override def fromBytes(bytes : Seq[Byte]) = Sha256Hash160DigestImpl(bytes)
}