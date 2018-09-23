package org.bitcoins.core.number

import org.bitcoins.core.number.UInt8.toUInt8
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{ BitcoinSUtil, Factory, NumberUtil }
import scodec.bits.ByteVector

import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 6/4/16.
 */

/**
 * This abstract class is meant to represent a signed and unsigned number in C
 * This is useful for dealing with codebases/protocols that rely on C's
 * unsigned integer types
 */
sealed abstract class Number[T <: Number[T]] extends NetworkElement {
  type A = BigInt
  /** The underlying scala number used to to hold the number */
  protected def underlying: A

  def toInt: Int = toBigInt.bigInteger.intValueExact()
  def toLong: Long = toBigInt.bigInteger.longValueExact()
  def toBigInt: BigInt = underlying

  /**
   * This is used to determine the valid amount of bytes in a number
   * for instance a UInt8 has an andMask of 0xff
   * a UInt32 has an andMask of 0xffffffff
   */
  def andMask: BigInt
  /** Factory function to create the underlying T, for instance a UInt32 */
  def apply: A => T
  def +(num: T): T = apply(checkResult(underlying + num.underlying))
  def -(num: T): T = apply(checkResult(underlying - num.underlying))
  def *(num: T): T = apply(checkResult(underlying * num.underlying))
  def >(num: T): Boolean = underlying > num.underlying
  def >=(num: T): Boolean = underlying >= num.underlying
  def <(num: T): Boolean = underlying < num.underlying
  def <=(num: T): Boolean = underlying <= num.underlying

  def <<(num: Int): T = this.<<(apply(num))
  def >>(num: Int): T = this.>>(apply(num))

  def <<(num: T): T = {
    checkIfInt(num).map { _ =>
      apply((underlying << num.toInt) & andMask)
    }.get
  }

  def >>(num: T): T = {
    //this check is for weird behavior with the jvm and shift rights
    //https://stackoverflow.com/questions/47519140/bitwise-shift-right-with-long-not-equaling-zero/47519728#47519728
    if (num.toLong > 63) apply(0)
    else {
      checkIfInt(num).map { _ =>
        apply(underlying >> num.toInt)
      }.get
    }
  }

  def |(num: T): T = apply(checkResult(underlying | num.underlying))
  def &(num: T): T = apply(checkResult(underlying & num.underlying))
  def unary_- : T = apply(-underlying)
  private def checkResult(result: BigInt): A = {
    require((result & andMask) == result, "Result was out of bounds, got: " + result)
    result
  }

  /** Checks if the given nubmer is within range of a Int */
  private def checkIfInt(num: T): Try[Unit] = {
    if (num.toBigInt >= Int.MaxValue || num.toBigInt <= Int.MinValue) {
      Failure(new IllegalArgumentException("Num was not in range of int, got: " + num))
    } else {
      Success(Unit)
    }
  }

  override def bytes = BitcoinSUtil.decodeHex(hex)
}

/**
 * Represents a signed number in our number system
 * Instances of this are [[Int32]] or [[Int64]]
 */
sealed abstract class SignedNumber[T <: Number[T]] extends Number[T]

/**
 * Represents an unsigned number in our number system
 * Instances of this are [[UInt32]] or [[UInt64]]
 */
sealed abstract class UnsignedNumber[T <: Number[T]] extends Number[T]

sealed abstract class UInt5 extends UnsignedNumber[UInt5] {
  override def apply: A => UInt5 = UInt5(_)

  override def andMask: BigInt = 0x1f

  def byte: Byte = toInt.toByte

  def toUInt8: UInt8 = UInt8(toInt)

  override def hex: String = toUInt8.hex
}

sealed abstract class UInt8 extends UnsignedNumber[UInt8] {
  override def apply: A => UInt8 = UInt8(_)

  override def hex = BitcoinSUtil.encodeHex(toInt.toShort).slice(2, 4)

  override def andMask = 0xff

  def toUInt5: UInt5 = {
    //this will throw if not in range of a UInt5, come back and look later
    UInt5(toInt)
  }
}

/**
 * Represents a uint32_t in C
 */
sealed abstract class UInt32 extends UnsignedNumber[UInt32] {
  override def apply: A => UInt32 = UInt32(_)
  override def hex = BitcoinSUtil.encodeHex(toLong).slice(8, 16)

  override def andMask = 0xffffffffL
}

/**
 * Represents a uint64_t in C
 */
sealed abstract class UInt64 extends UnsignedNumber[UInt64] {
  override def hex = encodeHex(underlying)
  override def apply: A => UInt64 = UInt64(_)
  override def andMask = 0xffffffffffffffffL

  /**
   * The converts a [[BigInt]] to a 8 byte hex representation
   * [[BigInt]] will only allocate 1 byte for numbers like 1 which require 1 byte, giving us the hex representation 01
   * this function pads the hex chars to be 0000000000000001
   * @param bigInt
   * @return
   */
  private def encodeHex(bigInt: BigInt): String = {
    val hex = BitcoinSUtil.encodeHex(bigInt)
    if (hex.length == 18) {
      //means that encodeHex(BigInt) padded an extra byte, giving us 9 bytes instead of 8
      hex.slice(2, hex.length)
    } else {
      val padding = for { _ <- 0 until 16 - hex.length } yield "0"
      padding.mkString ++ hex
    }
  }
}

/**
 * Represents a int32_t in C
 */
sealed abstract class Int32 extends SignedNumber[Int32] {
  override def apply: A => Int32 = Int32(_)
  override def andMask = 0xffffffff
  override def hex = BitcoinSUtil.encodeHex(toInt)
}

/**
 * Represents a int64_t in C
 */
sealed abstract class Int64 extends SignedNumber[Int64] {
  override def apply: A => Int64 = Int64(_)
  override def andMask = 0xffffffffffffffffL
  override def hex = BitcoinSUtil.encodeHex(toLong)
}

/**
 * Represents various numbers that should be implemented
 * inside of any companion object for a number
 */
trait BaseNumbers[T] {
  def zero: T
  def one: T
  def min: T
  def max: T
}

object UInt5 extends Factory[UInt5] with BaseNumbers[UInt5] {
  private case class UInt5Impl(underlying: BigInt) extends UInt5 {
    require(underlying.toInt >= 0, s"Cannot create UInt5 from $underlying")
    require(underlying <= 31, s"Cannot create UInt5 from $underlying")
  }

  lazy val zero = UInt5(0.toByte)
  lazy val one = UInt5(1.toByte)

  lazy val min = zero
  lazy val max = UInt5(31.toByte)

  def apply(byte: Byte): UInt5 = fromByte(byte)

  def apply(bigInt: BigInt): UInt5 = {

    require(
      bigInt.toByteArray.size == 1,
      s"To create a uint5 from a ByteVector it must be of size one ${bigInt}")

    UInt5.fromByte(bigInt.toByteArray.head)
  }

  override def fromBytes(bytes: ByteVector): UInt5 = {
    require(bytes.size == 1, s"To create a uint5 from a ByteVector it must be of size one ${bytes.length}")
    UInt5.fromByte(bytes.head)
  }

  def fromByte(byte: Byte): UInt5 = {
    UInt5Impl(BigInt(byte))
  }

  def toUInt5(b: Byte): UInt5 = {
    fromByte(b)
  }

  def toUInt5s(bytes: ByteVector): Vector[UInt5] = {
    bytes.toArray.map(toUInt5(_)).toVector
  }
}

object UInt8 extends Factory[UInt8] with BaseNumbers[UInt8] {
  private case class UInt8Impl(underlying: BigInt) extends UInt8 {
    require(isValid(underlying), "Invalid range for a UInt8, got: " + underlying)
  }
  lazy val zero = UInt8(0.toShort)
  lazy val one = UInt8(1.toShort)

  lazy val min = zero
  lazy val max = UInt8(255.toShort)

  def apply(short: Short): UInt8 = UInt8(BigInt(short))

  def apply(byte: Byte): UInt8 = toUInt8(byte)

  def apply(bigint: BigInt): UInt8 = UInt8Impl(bigint)

  def isValid(bigInt: BigInt): Boolean = bigInt >= 0 && bigInt < 256

  override def fromBytes(bytes: ByteVector): UInt8 = {
    require(bytes.size == 1, "Can only create a uint8 from a byte array of size one, got: " + bytes)
    val res = NumberUtil.toUnsignedInt(bytes)
    checkBounds(res)
  }

  def toUInt8(byte: Byte): UInt8 = {
    fromBytes(ByteVector.fromByte(byte))
  }

  def toByte(uInt8: UInt8): Byte = uInt8.underlying.toByte

  def toBytes(us: Seq[UInt8]): ByteVector = {
    ByteVector(us.map(toByte(_)))
  }

  def toUInt8s(bytes: ByteVector): Vector[UInt8] = {
    bytes.toArray.map(toUInt8(_)).toVector
  }

  def checkBounds(res: BigInt): UInt8 = {
    if (res > max.underlying || res < min.underlying) {
      throw new IllegalArgumentException("Out of boudns for a UInt8, got: " + res)
    } else UInt8(res.toShort)
  }
}

object UInt32 extends Factory[UInt32] with BaseNumbers[UInt32] {
  private case class UInt32Impl(underlying: BigInt) extends UInt32 {
    require(underlying >= 0, "We cannot have a negative number in an unsigned number, got: " + underlying)
    require(underlying <= 4294967295L, "We cannot have a number larger than 2^32 -1 in UInt32, got: " + underlying)
  }

  lazy val zero = UInt32(0)
  lazy val one = UInt32(1)

  lazy val min = zero
  lazy val max = UInt32(4294967295L)

  override def fromBytes(bytes: ByteVector): UInt32 = {
    require(bytes.size <= 4, "UInt32 byte array was too large, got: " + BitcoinSUtil.encodeHex(bytes))
    val res = NumberUtil.toUnsignedInt(bytes)
    checkBounds(res)
  }

  def apply(long: Long): UInt32 = UInt32(BigInt(long))

  def apply(bigInt: BigInt): UInt32 = UInt32Impl(bigInt)

  def checkBounds(res: BigInt): UInt32 = {
    if (res > max.underlying || res < min.underlying) {
      throw new IllegalArgumentException("Out of boudns for a UInt8, got: " + res)
    } else UInt32(res)
  }

}

object UInt64 extends Factory[UInt64] with BaseNumbers[UInt64] {
  private case class UInt64Impl(underlying: BigInt) extends UInt64 {
    require(underlying >= 0, "We cannot have a negative number in an unsigned number: " + underlying)
    require(underlying <= BigInt("18446744073709551615"), "We cannot have a number larger than 2^64 -1 in UInt64, got: " + underlying)
  }

  lazy val zero = UInt64(BigInt(0))
  lazy val one = UInt64(BigInt(1))

  lazy val min = zero
  lazy val max = UInt64(BigInt("18446744073709551615"))

  override def fromBytes(bytes: ByteVector): UInt64 = {
    require(bytes.size <= 8)
    val res: BigInt = NumberUtil.toUnsignedInt(bytes)
    if (res > max.underlying || res < min.underlying) {
      throw new IllegalArgumentException("Out of bounds for a UInt64, got: " + res)
    } else UInt64(res)
  }

  def apply(num: BigInt): UInt64 = UInt64Impl(num)

}

object Int32 extends Factory[Int32] with BaseNumbers[Int32] {
  private case class Int32Impl(underlying: BigInt) extends Int32 {
    require(underlying >= -2147483648, "Number was too small for a int32, got: " + underlying)
    require(underlying <= 2147483647, "Number was too large for a int32, got: " + underlying)
  }

  lazy val zero = Int32(0)
  lazy val one = Int32(1)

  lazy val min = Int32(-2147483648)
  lazy val max = Int32(2147483647)

  override def fromBytes(bytes: ByteVector): Int32 = {
    require(bytes.size <= 4, "We cannot have an Int32 be larger than 4 bytes")
    Int32(BigInt(bytes.toArray).toInt)
  }

  def apply(int: Int): Int32 = Int32(BigInt(int))

  def apply(bigInt: BigInt): Int32 = Int32Impl(bigInt)
}

object Int64 extends Factory[Int64] with BaseNumbers[Int64] {
  private case class Int64Impl(underlying: BigInt) extends Int64 {
    require(underlying >= -9223372036854775808L, "Number was too small for a int64, got: " + underlying)
    require(underlying <= 9223372036854775807L, "Number was too big for a int64, got: " + underlying)
  }

  lazy val zero = Int64(0)
  lazy val one = Int64(1)

  lazy val min = Int64(-9223372036854775808L)
  lazy val max = Int64(9223372036854775807L)

  override def fromBytes(bytes: ByteVector): Int64 = {
    require(bytes.size <= 8, "We cannot have an Int64 be larger than 8 bytes")
    Int64(BigInt(bytes.toArray).toLong)
  }

  def apply(long: Long): Int64 = Int64(BigInt(long))

  def apply(bigInt: BigInt): Int64 = Int64Impl(bigInt)
}
