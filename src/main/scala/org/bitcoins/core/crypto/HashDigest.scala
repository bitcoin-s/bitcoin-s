package org.bitcoins.core.crypto

import org.bitcoins.core.util.{ Factory, BitcoinSUtil }

/**
 * Created by chris on 5/24/16.
 */
sealed abstract class HashDigest {
  /** The message digest represented in bytes */
  def bytes: Seq[Byte]

  /** The message digest represented in hexadecimal */
  def hex: String = BitcoinSUtil.encodeHex(bytes)

  /**
   * Flips the endianness of the byte sequence.
   * Since bitcoin unfortunately has inconsistent endianness between the protocol
   * level and the presentation level. This is useful for switching between the two.
   * @return
   */
  def flip: HashDigest
}

/**
 * Represents the result of SHA1()
 */
sealed abstract class Sha1Digest extends HashDigest {
  override def flip: Sha1Digest = Sha1Digest(bytes.reverse)
}

object Sha1Digest extends Factory[Sha1Digest] {
  private case class Sha1DigestImpl(bytes: Seq[Byte]) extends Sha1Digest {
    override def toString = s"Sha1DigestImpl($hex)"
  }
  override def fromBytes(bytes: Seq[Byte]): Sha1Digest = Sha1DigestImpl(bytes)
}

/**
 * Represents the result of SHA256()
 */
sealed abstract class Sha256Digest extends HashDigest {
  override def flip: Sha256Digest = Sha256Digest(bytes.reverse)
}

object Sha256Digest extends Factory[Sha256Digest] {
  private case class Sha256DigestImpl(bytes: Seq[Byte]) extends Sha256Digest {
    require(bytes.length == 32, "Sha256Digest must be 32 bytes in size, got: " + bytes.length)
    override def toString = s"Sha256DigestImpl($hex)"
  }
  override def fromBytes(bytes: Seq[Byte]): Sha256Digest = Sha256DigestImpl(bytes)
}

/**
 * Represents the result of SHA256(SHA256())
 */
sealed abstract class DoubleSha256Digest extends HashDigest {
  def flip: DoubleSha256Digest = DoubleSha256Digest(bytes.reverse)
}

object DoubleSha256Digest extends Factory[DoubleSha256Digest] {
  private case class DoubleSha256DigestImpl(bytes: Seq[Byte]) extends DoubleSha256Digest {
    require(bytes.length == 32, "DoubleSha256Digest must always be 32 bytes, got: " + bytes.length)
    override def toString = s"DoubleSha256DigestImpl($hex)"
  }
  override def fromBytes(bytes: Seq[Byte]): DoubleSha256Digest = DoubleSha256DigestImpl(bytes)

}

/**
 * Represents the result of RIPEMD160()
 */
sealed abstract class RipeMd160Digest extends HashDigest {
  override def flip: RipeMd160Digest = RipeMd160Digest(bytes.reverse)
}

object RipeMd160Digest extends Factory[RipeMd160Digest] {
  private case class RipeMd160DigestImpl(bytes: Seq[Byte]) extends RipeMd160Digest {
    require(bytes.length == 20, "RIPEMD160Digest must always be 20 bytes, got: " + bytes.length)
    override def toString = s"RipeMd160DigestImpl($hex)"
  }
  override def fromBytes(bytes: Seq[Byte]): RipeMd160Digest = RipeMd160DigestImpl(bytes)
}

/**
 * Represents the result of RIPEMD160(SHA256())
 */
sealed abstract class Sha256Hash160Digest extends HashDigest {
  override def flip: Sha256Hash160Digest = Sha256Hash160Digest(bytes.reverse)
}

object Sha256Hash160Digest extends Factory[Sha256Hash160Digest] {
  private case class Sha256Hash160DigestImpl(bytes: Seq[Byte]) extends Sha256Hash160Digest {
    require(bytes.length == 20, "Sha256Hash160Digest must always be 20 bytes, got: " + bytes.length)
    override def toString = s"Sha256Hash160DigestImpl($hex)"
  }
  override def fromBytes(bytes: Seq[Byte]): Sha256Hash160Digest = Sha256Hash160DigestImpl(bytes)
}