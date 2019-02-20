package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

/**
  * Created by chris on 5/24/16.
  */
sealed abstract class HashDigest extends NetworkElement {

  /** The message digest represented in bytes */
  def bytes: ByteVector

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
  private case class Sha1DigestImpl(bytes: ByteVector) extends Sha1Digest {
    // $COVERAGE-OFF$
    override def toString = s"Sha1DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): Sha1Digest = Sha1DigestImpl(bytes)
}

/**
  * Represents the result of SHA256()
  */
sealed abstract class Sha256Digest extends HashDigest {
  override def flip: Sha256Digest = Sha256Digest(bytes.reverse)
}

object Sha256Digest extends Factory[Sha256Digest] {
  private case class Sha256DigestImpl(bytes: ByteVector) extends Sha256Digest {
    require(bytes.length == 32,
            // $COVERAGE-OFF$
            "Sha256Digest must be 32 bytes in size, got: " + bytes.length)
    override def toString = s"Sha256DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): Sha256Digest =
    Sha256DigestImpl(bytes)
}

/**
  * Represents the result of SHA256(SHA256())
  */
sealed abstract class DoubleSha256Digest extends HashDigest {
  def flip: DoubleSha256Digest = DoubleSha256Digest(bytes.reverse)
}

object DoubleSha256Digest extends Factory[DoubleSha256Digest] {
  private case class DoubleSha256DigestImpl(bytes: ByteVector)
      extends DoubleSha256Digest {
    require(bytes.length == 32,
            // $COVERAGE-OFF$
            "DoubleSha256Digest must always be 32 bytes, got: " + bytes.length)
    override def toString = s"DoubleSha256DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): DoubleSha256Digest =
    DoubleSha256DigestImpl(bytes)

}

/**
  * Represents the result of RIPEMD160()
  */
sealed abstract class RipeMd160Digest extends HashDigest {
  override def flip: RipeMd160Digest = RipeMd160Digest(bytes.reverse)
}

object RipeMd160Digest extends Factory[RipeMd160Digest] {
  private case class RipeMd160DigestImpl(bytes: ByteVector)
      extends RipeMd160Digest {
    require(bytes.length == 20,
            // $COVERAGE-OFF$
            "RIPEMD160Digest must always be 20 bytes, got: " + bytes.length)
    override def toString = s"RipeMd160DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): RipeMd160Digest =
    RipeMd160DigestImpl(bytes)
}

/**
  * Represents the result of RIPEMD160(SHA256())
  */
sealed abstract class Sha256Hash160Digest extends HashDigest {
  override def flip: Sha256Hash160Digest = Sha256Hash160Digest(bytes.reverse)
}

object Sha256Hash160Digest extends Factory[Sha256Hash160Digest] {
  private case class Sha256Hash160DigestImpl(bytes: ByteVector)
      extends Sha256Hash160Digest {
    require(bytes.length == 20,
            // $COVERAGE-OFF$
            "Sha256Hash160Digest must always be 20 bytes, got: " + bytes.length)
    override def toString = s"Sha256Hash160DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): Sha256Hash160Digest =
    Sha256Hash160DigestImpl(bytes)
}
