package org.bitcoins.core.number

import java.math.BigInteger

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{BitcoinSUtil, Factory, NumberUtil}
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 6/4/16.
  */
/**
  * This abstract class is meant to represent a signed and unsigned number in C
  * This is useful for dealing with codebases/protocols that rely on C's
  * unsigned integer types
  */
sealed abstract class Number[T <: Number[T]]
    extends NetworkElement
    with Ordered[T]
    with BasicArithmetic[T] {
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

  /**
    * Factory function to create the underlying T, for instance a UInt32.
    * This method must check if the parameter is in the required range.
    */
  def apply: A => T

  override def +(num: T): T = apply(underlying + num.underlying)
  override def -(num: T): T = apply(underlying - num.underlying)
  override def *(factor: BigInt): T = apply(underlying * factor)
  override def *(num: T): T = apply(underlying * num.underlying)

  override def compare(num: T): Int = underlying compare num.underlying

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

  def |(num: T): T = apply(underlying | num.underlying)
  def &(num: T): T = apply(underlying & num.underlying)
  def unary_- : T = apply(-underlying)

  /** Checks if the given nubmer is within range of a Int */
  private def checkIfInt(num: T): Try[Unit] = {
    if (num.toBigInt >= Int.MaxValue || num.toBigInt <= Int.MinValue) {
      Failure(
        new IllegalArgumentException(
          "Num was not in range of int, got: " + num))
    } else {
      Success(())
    }
  }

  override def bytes: ByteVector = BitcoinSUtil.decodeHex(hex)
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

/** This number type is useful for dealing with [[org.bitcoins.core.util.Bech32]]
  * related applications. The native encoding for Bech32 is a 5 bit number which
  * is what this abstraction is meant to  be used for
  */
sealed abstract class UInt5 extends UnsignedNumber[UInt5] {
  override def apply: A => UInt5 = UInt5(_)

  override def andMask: BigInt = 0x1f

  def byte: Byte = toInt.toByte

  def toUInt8: UInt8 = UInt8(toInt)

  override def hex: String = toUInt8.hex
}

sealed abstract class UInt8 extends UnsignedNumber[UInt8] {
  override def apply: A => UInt8 = UInt8(_)

  override def hex: String = BitcoinSUtil.encodeHex(toInt.toShort).slice(2, 4)

  override def andMask = 0xff

  def toUInt5: UInt5 = {
    //this will throw if not in range of a UInt5, come back and look later
    UInt5(toInt)
  }
}

/**
  * Represents a uint32_t in C
  */
final case class UInt32 private (private val underlying: Long)
    extends AnyVal
    with NetworkElement
    with BasicArithmetic[UInt32]
    with Ordered[UInt32] {
  override def hex: String = BitcoinSUtil.encodeHex(underlying).slice(8, 16)

  override def bytes: ByteVector = ByteVector.fromValidHex(hex)

  override def +(y: UInt32): UInt32 = {
    UInt32(underlying + y.underlying)
  }

  override def -(y: UInt32): UInt32 = {
    UInt32(underlying - y.underlying)
  }

  override def *(y: UInt32): UInt32 = {
    UInt32(underlying * y.underlying)
  }

  override def *(factor: BigInt): UInt32 = ???

  def |(u32: UInt32): UInt32 = {
    UInt32(underlying | u32.underlying)
  }

  def &(u32: UInt32): UInt32 = {
    UInt32(underlying & u32.underlying)
  }

  def <<(u32: UInt32): UInt32 = {
    UInt32(underlying << u32.underlying)
  }

  def <<(int: Int): UInt32 = {
    this.<<(UInt32(int))
  }

  def >>(u32: UInt32): UInt32 = {
    UInt32(underlying >> u32.underlying)
  }

  def >>(int: Int): UInt32 = {
    UInt32(underlying >> int)
  }

  def toInt: Int = {
    val i = underlying.toInt
    require(underlying == i,
            s"Rounded when converting long=${underlying} toInt=${i}")
    i
  }

  def toLong: Long = underlying

  def toBigInt: BigInt = toLong

  def toDouble(x: UInt32): Double = x.underlying.toDouble
  def toFloat(x: UInt32): Float = x.underlying.toFloat

  def toInt(x: UInt32): Int = {
    val i = x.underlying.toInt
    require(x.underlying == i)
    i
  }
  def toLong(x: UInt32): Long = x.underlying

  override def compare(that: UInt32): Int = underlying.compare(that.underlying)
}

/**
  * Represents a uint64_t in C
  */
final case class UInt64 private (private val underlying: java.math.BigInteger)
    extends AnyVal
    with NetworkElement
    with BasicArithmetic[UInt64]
    with Ordered[UInt64] {

  override def hex: String = encodeHex(underlying)

  override def bytes: ByteVector = ByteVector.fromValidHex(hex)

  override def +(y: UInt64): UInt64 = {
    UInt64(underlying.add(y.underlying))
  }

  override def -(y: UInt64): UInt64 = {
    UInt64(underlying.subtract(y.underlying))
  }

  override def *(y: UInt64): UInt64 = {
    UInt64(underlying.multiply(y.underlying))
  }

  override def *(factor: BigInt): UInt64 = ???

  def |(u64: UInt64): UInt64 = {
    UInt64(underlying.or(u64.underlying))
  }

  def &(u64: UInt64): UInt64 = {
    UInt64(underlying.and(u64.underlying))
  }

  def <<(u64: UInt64): UInt64 = {
    UInt64(underlying.shiftLeft(u64.underlying.intValueExact()))
  }

  def >>(u64: UInt64): UInt64 = {
    UInt64(underlying.shiftRight(u64.underlying.intValueExact()))
  }

  def toBigInt: BigInteger = {
    val i = underlying
    require(underlying == i,
            s"Rounded when converting long=${underlying} toBigInt=${i}")
    i
  }

  def toBigInt: BigInteger = underlying

  def toLong: Long = underlying.longValueExact()

  def toDouble(x: UInt64): Double = x.underlying.doubleValue()
  def toFloat(x: UInt64): Float = x.underlying.floatValue()

  def toBigInt(x: UInt64): BigInt = x.underlying

  override def compare(that: UInt64): Int =
    underlying.compareTo(that.underlying)

  /**
    * Converts a [[BigInt]] to a 8 byte hex representation.
    * [[BigInt]] will only allocate 1 byte for numbers like 1 which require 1 byte, giving us the hex representation 01
    * this function pads the hex chars to be 0000000000000001
    *
    * @param bigInt The number to encode
    * @return The hex encoded number
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
final case class Int32 private (private val underlying: Int)
    extends AnyVal
    with NetworkElement
    with BasicArithmetic[Int32]
    with Ordered[Int32] {
  override def hex: String = BitcoinSUtil.encodeHex(underlying).slice(8, 16)

  override def bytes: ByteVector = ByteVector.fromValidHex(hex)

  override def +(y: Int32): Int32 = {
    Int32(underlying + y.underlying)
  }

  override def -(y: Int32): Int32 = {
    Int32(underlying - y.underlying)
  }

  override def *(y: Int32): Int32 = {
    Int32(underlying * y.underlying)
  }

  override def *(factor: BigInt): Int32 = ???

  def |(s32: Int32): Int32 = {
    Int32(underlying | s32.underlying)
  }

  def &(s32: Int32): Int32 = {
    Int32(underlying & s32.underlying)
  }

  def <<(s32: Int32): Int32 = {
    Int32(underlying << s32.underlying)
  }

  def <<(int: Int): Int32 = {
    this.<<(Int32(int))
  }

  def >>(s32: Int32): Int32 = {
    Int32(underlying >> s32.underlying)
  }

  def >>(int: Int): Int32 = {
    Int32(underlying >> int)
  }

  def toInt: Int = {
    val i = underlying.toInt
    require(underlying == i,
            s"Rounded when converting int=${underlying} toInt=${i}")
    i
  }

  def toBigInt: BigInt = toInt

  def toDouble(x: Int32): Double = x.underlying.toDouble
  def toFloat(x: Int32): Float = x.underlying.toFloat

  def toInt(x: Int32): Int = {
    val i = x.underlying
    require(x.underlying == i)
    i
  }
  def toLong(x: Int32): Long = x.underlying

  override def compare(that: Int32): Int = underlying.compare(that.underlying)
}

/**
  * Represents a int64_t in C
  */
sealed abstract class Int64 extends SignedNumber[Int64] {
  override def apply: A => Int64 = Int64(_)
  override def andMask = 0xffffffffffffffffL
  override def hex: String = BitcoinSUtil.encodeHex(toLong)
}

/**
  * Represents number types that are bounded by minimum and maximum values
  *
  * @tparam T Type of the numbers
  */
trait Bounded[T] {
  def min: T
  def max: T
}

trait BaseNumbers[T] {
  def zero: T
  def one: T
}

/**
  * Should be implemented inside of any companion
  * object for a number
  */
trait NumberObject[T <: Number[T]] extends BaseNumbers[T] {
  type A = BigInt

  def isInBound(num: A): Boolean
}

object UInt5
    extends Factory[UInt5]
    with NumberObject[UInt5]
    with Bounded[UInt5] {
  private case class UInt5Impl(underlying: BigInt) extends UInt5 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }

  lazy val zero = UInt5(0.toByte)
  lazy val one = UInt5(1.toByte)

  private lazy val minUnderlying: A = 0
  private lazy val maxUnderlying: A = 31

  lazy val min = UInt5(minUnderlying)
  lazy val max = UInt5(maxUnderlying)

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  def apply(byte: Byte): UInt5 = fromByte(byte)

  def apply(bigInt: BigInt): UInt5 = {
    require(
      bigInt.toByteArray.length == 1,
      s"To create a uint5 from a BigInt it must be less than 32. Got: ${bigInt.toString}")

    UInt5.fromByte(bigInt.toByteArray.head)
  }

  override def fromBytes(bytes: ByteVector): UInt5 = {
    require(
      bytes.size == 1,
      s"To create a uint5 from a ByteVector it must be of size one ${bytes.length}")
    UInt5.fromByte(bytes.head)
  }

  def fromByte(byte: Byte): UInt5 = {
    UInt5Impl(BigInt(byte))
  }

  def toUInt5(b: Byte): UInt5 = {
    fromByte(b)
  }

  def toUInt5s(bytes: ByteVector): Vector[UInt5] = {
    bytes.toArray.map(toUInt5).toVector
  }
}

object UInt8
    extends Factory[UInt8]
    with NumberObject[UInt8]
    with Bounded[UInt8] {
  private case class UInt8Impl(underlying: BigInt) extends UInt8 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }
  lazy val zero = UInt8(0.toShort)
  lazy val one = UInt8(1.toShort)

  private lazy val minUnderlying: A = 0
  private lazy val maxUnderlying: A = 255

  lazy val min = UInt8(minUnderlying)
  lazy val max = UInt8(maxUnderlying)

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  def apply(short: Short): UInt8 = UInt8(BigInt(short))

  def apply(byte: Byte): UInt8 = toUInt8(byte)

  def apply(bigint: BigInt): UInt8 = UInt8Impl(bigint)

  override def fromBytes(bytes: ByteVector): UInt8 = {
    require(
      bytes.size == 1,
      "Can only create a uint8 from a byte array of size one, got: " + bytes)
    UInt8(NumberUtil.toUnsignedInt(bytes))
  }

  def toUInt8(byte: Byte): UInt8 = {
    fromBytes(ByteVector.fromByte(byte))
  }

  def toByte(uInt8: UInt8): Byte = uInt8.underlying.toByte

  def toBytes(us: Seq[UInt8]): ByteVector = {
    ByteVector(us.map(toByte))
  }

  def toUInt8s(bytes: ByteVector): Vector[UInt8] = {
    bytes.toArray.map(toUInt8).toVector
  }
}

object UInt32
    extends Factory[UInt32]
    /*    with NumberObject[UInt32]*/
    with Bounded[UInt32] {
  /*  private case class UInt32Impl(underlying: BigInt) extends UInt32 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }*/

  lazy val zero = UInt32(0)
  lazy val one = UInt32(1)

  private lazy val minUnderlying = 0
  private lazy val maxUnderlying = 4294967295L

  lazy val min = UInt32(minUnderlying)
  lazy val max = UInt32(maxUnderlying)

  def isInBound(num: Long): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  override def fromBytes(bytes: ByteVector): UInt32 = {
    require(
      bytes.size <= 4,
      "UInt32 byte array was too large, got: " + BitcoinSUtil.encodeHex(bytes))
  }

  def apply(bigInt: BigInt): UInt32 = {
    require(isInBound(bigInt.toLong),
            "UInt32 number is not within the max/min bounds allowed")
    UInt32(bigInt.bigInteger.longValueExact())
  }
}

object UInt64
    extends Factory[UInt64]
    /* with NumberObject[UInt64] */
    with Bounded[UInt64] {
  /* private case class UInt64Impl(underlying: BigInt) extends UInt64 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }
   */
  lazy val zero = UInt64(BigInt(0))
  lazy val one = UInt64(BigInt(1))

  private lazy val minUnderlying = 0
  private lazy val maxUnderlying = BigInt("18446744073709551615")

  lazy val min = UInt64(minUnderlying)
  lazy val max = UInt64(maxUnderlying)

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  override def fromBytes(bytes: ByteVector): UInt64 = {
    require(bytes.size <= 8)
    UInt64(NumberUtil.toUnsignedInt(bytes))
  }

  def apply(num: BigInt): UInt64 = {
    require(isInBound(num) == true,
            "UInt64 number is not within the min/max bounds")
    UInt64(num.bigInteger)
  }
}

object Int32
    extends Factory[Int32]
    //with NumberObject[Int32]
    with Bounded[Int32] {
  /*private case class Int32Impl(underlying: BigInt) extends Int32 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }
   */
  lazy val zero = Int32(0)
  lazy val one = Int32(1)

  private lazy val minUnderlying = -2147483648
  private lazy val maxUnderlying = 2147483647

  lazy val min = Int32(minUnderlying)
  lazy val max = Int32(maxUnderlying)

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying */

  override def fromBytes(bytes: ByteVector): Int32 = {
    require(bytes.size <= 4, "We cannot have an Int32 be larger than 4 bytes")
    Int32(BigInt(bytes.toArray).toInt)
  }

  def apply(bigInt: BigInt): Int32 = {
    require(isInBound(bigInt) == true)
    Int32(bigInt.bigInteger.intValueExact())
  }
}

object Int64
    extends Factory[Int64]
    with NumberObject[Int64]
    with Bounded[Int64] {
  private case class Int64Impl(underlying: BigInt) extends Int64 {
    require(isInBound(underlying),
            s"Cannot create ${super.getClass.getSimpleName} from $underlying")
  }

  lazy val zero = Int64(0)
  lazy val one = Int64(1)

  private lazy val minUnderlying: A = -9223372036854775808L
  private lazy val maxUnderlying: A = 9223372036854775807L

  lazy val min = Int64(minUnderlying)
  lazy val max = Int64(maxUnderlying)

  override def isInBound(num: A): Boolean =
    num <= maxUnderlying && num >= minUnderlying

  override def fromBytes(bytes: ByteVector): Int64 = {
    require(bytes.size <= 8, "We cannot have an Int64 be larger than 8 bytes")
    Int64(BigInt(bytes.toArray).toLong)
  }

  def apply(long: Long): Int64 = Int64(BigInt(long))

  def apply(bigInt: BigInt): Int64 = Int64Impl(bigInt)
}
