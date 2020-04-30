package org.bitcoins.crypto

import scodec.bits.ByteVector

sealed trait HashDigest extends Any with NetworkElement {

  /** The message digest represented in bytes */
  override def bytes: ByteVector

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
sealed trait Sha1Digest extends Any with HashDigest {
  override def flip: Sha1DigestBE = Sha1DigestBE(bytes.reverse)
}

object Sha1Digest extends Factory[Sha1Digest] {
  private case class Sha1DigestImpl(bytes: ByteVector)
      extends AnyVal
      with Sha1Digest {
    // $COVERAGE-OFF$
    override def toString = s"Sha1DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): Sha1Digest = Sha1DigestImpl(bytes)
}

sealed trait Sha1DigestBE extends Any with HashDigest {
  override def flip: Sha1Digest = Sha1Digest(bytes.reverse)
}

object Sha1DigestBE extends Factory[Sha1DigestBE] {
  private case class Sha1DigestBEImpl(bytes: ByteVector)
      extends AnyVal
      with Sha1DigestBE {
    // $COVERAGE-OFF$
    override def toString = s"Sha1DigestBEImpl($hex)"
    // $COVERAGE-ON$
  }

  override def fromBytes(bytes: ByteVector): Sha1DigestBE = {
    Sha1DigestBEImpl(bytes)
  }
}

/**
  * Represents the result of SHA256()
  */
sealed trait Sha256Digest extends Any with HashDigest {
  override def flip: Sha256DigestBE = Sha256DigestBE(bytes.reverse)
}

object Sha256Digest extends Factory[Sha256Digest] {
  private case class Sha256DigestImpl(bytes: ByteVector)
      extends AnyVal
      with Sha256Digest {
    override def toString = s"Sha256DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): Sha256Digest = {
    require(bytes.length == 32,
            // $COVERAGE-OFF$
            "Sha256Digest must be 32 bytes in size, got: " + bytes.length)
    Sha256DigestImpl(bytes)
  }

  private val e = ByteVector(Array.fill(32)(0.toByte))

  val empty: Sha256Digest = Sha256Digest.fromBytes(e)

}

/**
  * Represents the result of SHA256()
  */
sealed trait Sha256DigestBE extends Any with HashDigest {
  override def flip: Sha256Digest = Sha256Digest(bytes.reverse)
}

object Sha256DigestBE extends Factory[Sha256DigestBE] {
  private case class Sha256DigestBEImpl(bytes: ByteVector)
      extends AnyVal
      with Sha256DigestBE {
    override def toString = s"Sha256DigestBEImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): Sha256DigestBE = {
    require(bytes.length == 32,
            // $COVERAGE-OFF$
            "Sha256Digest must be 32 bytes in size, got: " + bytes.length)
    Sha256DigestBEImpl(bytes)
  }
}

/**
  * Represents the result of SHA256(SHA256())
  */
case class DoubleSha256Digest(bytes: ByteVector) extends HashDigest {
  require(bytes.length == 32,
          "DoubleSha256Digest must always be 32 bytes, got: " + bytes.length)

  lazy val flip: DoubleSha256DigestBE = DoubleSha256DigestBE(bytes.reverse)

  override def toString = s"DoubleSha256Digest($hex)"
}

object DoubleSha256Digest extends Factory[DoubleSha256Digest] {
  override def fromBytes(bytes: ByteVector): DoubleSha256Digest = {
    // have to use new to avoid infinite loop
    new DoubleSha256Digest(bytes)
  }

  val empty: DoubleSha256Digest = DoubleSha256Digest(
    ByteVector.low(32)
  )

}

/** The big endian version of [[DoubleSha256Digest DoubleSha256Digest]] */
case class DoubleSha256DigestBE(bytes: ByteVector) extends HashDigest {
  require(bytes.length == 32,
          "DoubleSha256Digest must always be 32 bytes, got: " + bytes.length)

  def flip: DoubleSha256Digest =
    DoubleSha256Digest.fromBytes(bytes.reverse)

  override def toString = s"DoubleSha256BDigestBE($hex)"
}

object DoubleSha256DigestBE extends Factory[DoubleSha256DigestBE] {
  override def fromBytes(bytes: ByteVector): DoubleSha256DigestBE =
    // have to use new to avoid infinite loop
    new DoubleSha256DigestBE(bytes)

  val empty: DoubleSha256DigestBE = DoubleSha256DigestBE(ByteVector.low(32))
}

/**
  * Represents the result of RIPEMD160()
  */
sealed trait RipeMd160Digest extends Any with HashDigest {
  override def flip: RipeMd160DigestBE = RipeMd160DigestBE(bytes.reverse)
}

object RipeMd160Digest extends Factory[RipeMd160Digest] {
  private case class RipeMd160DigestImpl(bytes: ByteVector)
      extends AnyVal
      with RipeMd160Digest {
    override def toString = s"RipeMd160DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): RipeMd160Digest = {
    require(bytes.length == 20,
            // $COVERAGE-OFF$
            "RIPEMD160Digest must always be 20 bytes, got: " + bytes.length)
    RipeMd160DigestImpl(bytes)
  }
}

/**
  * Represents the result of RIPEMD160() big endian
  */
sealed trait RipeMd160DigestBE extends Any with HashDigest {
  override def flip: RipeMd160Digest = RipeMd160Digest(bytes.reverse)
}

object RipeMd160DigestBE extends Factory[RipeMd160DigestBE] {
  private case class RipeMd160DigestBEImpl(bytes: ByteVector)
      extends AnyVal
      with RipeMd160DigestBE {
    override def toString = s"RipeMd160DigestBEImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): RipeMd160DigestBE = {
    require(bytes.length == 20,
            // $COVERAGE-OFF$
            "RIPEMD160Digest must always be 20 bytes, got: " + bytes.length)
    RipeMd160DigestBEImpl(bytes)
  }
}

/**
  * Represents the result of RIPEMD160(SHA256())
  */
sealed trait Sha256Hash160Digest extends Any with HashDigest {
  override def flip: Sha256Hash160DigestBE =
    Sha256Hash160DigestBE(bytes.reverse)
}

object Sha256Hash160Digest extends Factory[Sha256Hash160Digest] {
  private case class Sha256Hash160DigestImpl(bytes: ByteVector)
      extends AnyVal
      with Sha256Hash160Digest {
    override def toString = s"Sha256Hash160DigestImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): Sha256Hash160Digest = {
    require(bytes.length == 20,
            // $COVERAGE-OFF$
            "Sha256Hash160Digest must always be 20 bytes, got: " + bytes.length)
    Sha256Hash160DigestImpl(bytes)
  }
}

/**
  * Represents the result of RIPEMD160(SHA256()) big endian
  */
sealed trait Sha256Hash160DigestBE extends Any with HashDigest {
  override def flip: Sha256Hash160Digest = Sha256Hash160Digest(bytes.reverse)
}

object Sha256Hash160DigestBE extends Factory[Sha256Hash160DigestBE] {
  private case class Sha256Hash160DigestBEImpl(bytes: ByteVector)
      extends AnyVal
      with Sha256Hash160DigestBE {
    override def toString = s"Sha256Hash160DigestBEImpl($hex)"
    // $COVERAGE-ON$
  }
  override def fromBytes(bytes: ByteVector): Sha256Hash160DigestBE = {
    require(bytes.length == 20,
            // $COVERAGE-OFF$
            "Sha256Hash160Digest must always be 20 bytes, got: " + bytes.length)
    Sha256Hash160DigestBEImpl(bytes)
  }
}
