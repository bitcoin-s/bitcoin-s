package org.bitcoins.core.number

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{BitcoinSUtil, Factory, NumberUtil}

import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 6/4/16.
  */

/**
  * A number can either be a signed or an unsigned number
  */
sealed trait Number extends NetworkElement {
  type A
  def underlying : A
  def toInt : Int
}

/**
  * Represents a signed number in our number system
  * Instances of this are [[Int32]] or [[Int64]]
  */
sealed trait SignedNumber extends Number

/**
  * Represents an unsigned number in our number system
  * Instances of this are [[UInt32]] or [[UInt64]]
  */
sealed trait UnsignedNumber extends Number

/**
  * This trait represents all numeric operations we have for [[Number]]
  * @tparam T the type our numeric operations return
  *           currently either an [[UnsignedNumber]] or [[SignedNumber]]
  */
sealed trait NumberOperations[T <: Number] {
  def + (num : T): T
  def - (num : T): T
  def * (num : T): T
  def > (num : T): Boolean
  def >= (num : T): Boolean
  def < (num : T): Boolean
  def <= (num : T): Boolean
}

sealed abstract class UInt8 extends UnsignedNumber with NumberOperations[UInt8] {
  override type A = Short

  override def + (num : UInt8): UInt8 = {
    val sum = underlying + num.underlying
    checkResult(sum)
  }

  override def - (num : UInt8): UInt8 =  {
    val difference = underlying - num.underlying
    checkResult(difference)
  }

  override def * (num : UInt8): UInt8 =  {
    val product = underlying * num.underlying
    checkResult(product)
  }

  override def > (num : UInt8): Boolean = underlying > num.underlying

  override def >= (num : UInt8): Boolean = underlying >= num.underlying

  override def < (num : UInt8): Boolean = underlying < num.underlying

  override def <= (num : UInt8): Boolean = underlying <= num.underlying

  def | (num : UInt8) : UInt8 = {
    val bitwiseOr = underlying | num.underlying
    checkResult(bitwiseOr)
  }

  def & (num : UInt8) : UInt8 = {
    val bitwiseAnd = underlying & num.underlying
    checkResult(bitwiseAnd)
  }

  def >> (u: UInt8): UInt8 = this.>>(u.underlying)
  def >> (i : Int): UInt8 = {
    val r = underlying >> i
    checkResult(r)
  }

  def <<(u: UInt8): UInt8 = this.<<(u.underlying)
  def << (i: Int): UInt8 = {
    val r = (underlying << i) & 0xffL
    checkResult(r)
  }
  override def hex = BitcoinSUtil.encodeHex(underlying).slice(2,4)

  override def toInt: Int = toLong.toInt

  def toLong: Long = underlying

  /**
    * Checks the result of the arithmetic operation to see if an error occurred
    * if an error does occur throw it, else return the [[UInt32]]
    * @param result the try type wrapping the result of the arithmetic operation
    * @return the result of the unsigned number operation
    */
  private def checkResult(result : Long): UInt8 = {
    if (result > UInt8.max.underlying || result < UInt8.min.underlying) throw new IllegalArgumentException("Result of operation was out of bounds for a UInt8: " + result)
    else UInt8(result.toShort)
  }
}

/**
  * Represents a uint32_t in C
  */
sealed trait UInt32 extends UnsignedNumber with NumberOperations[UInt32] {
  override type A = Long

  override def + (num : UInt32): UInt32 = {
    val sum = underlying + num.underlying
    val result = Try(UInt32(sum))
    checkResult(result)
  }

  override def - (num : UInt32): UInt32 =  {
    val difference = underlying - num.underlying
    val result = Try(UInt32(difference))
    checkResult(result)
  }

  override def * (num : UInt32): UInt32 =  {
    val product = underlying * num.underlying
    val result = Try(UInt32(product))
    checkResult(result)
  }

  override def > (num : UInt32): Boolean = underlying > num.underlying

  override def >= (num : UInt32): Boolean = underlying >= num.underlying

  override def < (num : UInt32): Boolean = underlying < num.underlying

  override def <= (num : UInt32): Boolean = underlying <= num.underlying

  def | (num : UInt32) : UInt32 = UInt32(underlying | num.underlying)

  def & (num : UInt32) : UInt32 = UInt32(underlying & num.underlying)

  def >> (u: UInt32): UInt32 = this.>>(u.underlying)
  def >> (l: Long): UInt32 = {
    val r = Try(UInt32(underlying >> l))
    checkResult(r)
  }

  def <<(u: UInt32): UInt32 = this.<<(u.underlying)
  def << (l: Long): UInt32 = {
    if (l == 0) this
    else {
      //since we are going to shift left we can lose precision by converting .toInt
      val int = underlying.toInt
      val shiftNoSignBit = (int << l) & 0xffffffffL
      val shift = if (((shiftNoSignBit & (1 << 31)) == (1 << 31))) {
        shiftNoSignBit + (1 << 31)
      } else shiftNoSignBit
      val r = Try(UInt32(shift))
      checkResult(r)
    }
  }

  override def hex = BitcoinSUtil.encodeHex(underlying).slice(8,16)

  override def toInt: Int = {
    require(underlying <= Int.MaxValue, "Overflow error when casting " + this + " to an integer.")
    require(underlying >= 0, "Unsigned integer should not be cast to a number less than 0" + this)
    underlying.toInt
  }

  def toLong: Long = {
    require(underlying <= Long.MaxValue, "Overflow error when casting " + this + " to an integer.")
    require(underlying >= 0, "Unsigned integer should not be cast to a number less than 0" + this)
    underlying.toLong
  }

  /**
    * Checks the result of the arithmetic operation to see if an error occurred
    * if an error does occur throw it, else return the [[UInt32]]
    * @param result the try type wrapping the result of the arithmetic operation
    * @return the result of the unsigned number operation
    */
  private def checkResult(result : Try[UInt32]): UInt32 = result match {
    case Success(number) => number
    case Failure(exception) => throw exception
  }
}

/**
  * Represents a uint64_t in C
  */
sealed trait UInt64 extends UnsignedNumber with NumberOperations[UInt64] {
  override type A = BigInt
  override def hex = encodeHex(underlying)

  override def + (num : UInt64): UInt64 = {
    val sum = underlying + num.underlying
    val result = Try(UInt64(sum))
    checkResult(result)
  }

  override def - (num : UInt64): UInt64 = {
    val difference = underlying - num.underlying
    val result = Try(UInt64(difference))
    checkResult(result)
  }

  override def * (num : UInt64): UInt64 = {
    val product = underlying * num.underlying
    val result = Try(UInt64(product))
    checkResult(result)
  }

  override def > (num : UInt64): Boolean = underlying > num.underlying

  override def >= (num : UInt64): Boolean = underlying >= num.underlying

  override def < (num : UInt64): Boolean = underlying < num.underlying

  override def <= (num : UInt64): Boolean = underlying <= num.underlying

  def | (num : UInt64) : UInt64 = UInt64(underlying | num.underlying)

  def & (num : UInt64) : UInt64 = UInt64(underlying & num.underlying)


  override def toInt = {
    require(underlying <= Int.MaxValue, "Overflow error when casting " + this + " to an integer.")
    require(underlying >= 0, "Unsigned integer should not be cast to a number less than 0" + this)
    underlying.toInt
  }

  /**
    * Checks the result of the arithmetic operation to see if an error occurred
    * if an error does occur throw it, else return the [[UInt64]]
    * @param result the try type wrapping the result of the arithmetic operation
    * @return the result of the unsigned number operation
    */
  private def checkResult(result : Try[UInt64]): UInt64 = result match {
    case Success(number) => number
    case Failure(exception) => throw exception
  }

  /**
    * The converts a [[BigInt]] to a 8 byte hex representation
    * [[BigInt]] will only allocate 1 byte for numbers like 1 which require 1 byte, giving us the hex representation 01
    * this function pads the hex chars to be 0000000000000001
    * @param bigInt
    * @return
    */
  private def encodeHex(bigInt : BigInt): String = {
    val hex = BitcoinSUtil.encodeHex(bigInt)
    if (hex.length == 18) {
      //means that encodeHex(BigInt) padded an extra byte, giving us 9 bytes instead of 8
      hex.slice(2,hex.length)
    } else {
      val padding = for { _ <- 0 until 16 - hex.length} yield "0"
      padding.mkString ++ hex
    }

  }
}

/**
  * Represents a int32_t in C
  */
sealed trait Int32 extends SignedNumber with NumberOperations[Int32] {
  override type A = Int
  override def + (num : Int32) = {
    val sum = underlying + num.underlying
    val result = Try(Int32(sum))
    checkResult(result)
  }
  override def - (num : Int32) = {
    val difference = underlying - num.underlying
    val result = Try(Int32(difference))
    checkResult(result)
  }

  override def *(num : Int32) = {
    val product = underlying * num.underlying
    val result = Try(Int32(product))
    checkResult(result)
  }

  override def > (num : Int32): Boolean = underlying > num.underlying

  override def >= (num : Int32): Boolean = underlying >= num.underlying

  override def < (num : Int32): Boolean = underlying < num.underlying

  override def <= (num : Int32): Boolean = underlying <= num.underlying

  def unary_- : Int32 = Int32(-underlying)
  def | (num : Int32) : Int32 = Int32(underlying | num.underlying)

  def & (num : Int32) : Int32 = Int32(underlying & num.underlying)

  override def toInt = {
    require(underlying <= Int.MaxValue, "Overflow error when casting " + this + " to an integer.")
    require(underlying >= Int.MinValue, "Overflow error when casting " + this + " to an integer.")
    underlying
  }

  /**
    * Checks the result of the arithmetic operation to see if an error occurred
    * if an error does occur throw it, else return the [[Int32]]
    * @param result the try type wrapping the result of the arithmetic operation
    * @return the result of the unsigned number operation
    */
  private def checkResult(result : Try[Int32]): Int32 = result match {
    case Success(number) => number
    case Failure(exception) => throw exception
  }

  override def hex = BitcoinSUtil.encodeHex(underlying)

}

/**
  * Represents a int64_t in C
  */
sealed trait Int64 extends SignedNumber with NumberOperations[Int64] {
  override type A = Long
  override def + (num : Int64) = {
    val sum = underlying + num.underlying
    val result = Try(Int64(sum))
    checkResult(result)
  }
  override def - (num : Int64) = {
    val difference = underlying - num.underlying
    val result = Try(Int64(difference))
    checkResult(result)
  }
  override def * (num : Int64) = {
    val product = underlying * num.underlying
    val result = Try(Int64(product))
    checkResult(result)
  }

  override def > (num : Int64): Boolean = underlying > num.underlying

  override def >= (num : Int64): Boolean = underlying >= num.underlying

  override def < (num : Int64): Boolean = underlying < num.underlying

  override def <= (num : Int64): Boolean = underlying <= num.underlying

  def unary_- : Int64 = Int64(-underlying)

  def | (num : Int64) : Int64 = Int64(underlying | num.underlying)

  def & (num : Int64) : Int64 = Int64(underlying & num.underlying)

  override def toInt = {
    require(underlying <= Int.MaxValue, "Overflow error when casting " + this + " to an integer.")
    require(underlying >= Int.MinValue, "Overflow error when casting " + this  + " to an integer.")
    underlying.toInt
  }


  /**
    * Checks the result of the arithmetic operation to see if an error occurred
    * if an error does occur throw it, else return the [[Int64]]
    * @param result the try type wrapping the result of the arithmetic operation
    * @return the result of the unsigned number operation
    */
  private def checkResult(result : Try[Int64]): Int64 = result match {
    case Success(number) => number
    case Failure(exception) => throw exception
  }

  override def hex = BitcoinSUtil.encodeHex(underlying)
}


object SignedNumber extends Factory[SignedNumber] {
  override def fromBytes(bytes : Seq[Byte]): SignedNumber = {
    if (bytes.size <= 4) Int32(bytes)
    else Int64(bytes)
  }
}



object UnsignedNumber extends Factory[UnsignedNumber] {
  override def fromBytes(bytes : Seq[Byte]): UnsignedNumber = {
    if (bytes.size <= 4) UInt32(bytes)
    else UInt64(bytes)
  }
}

/**
  * Represents various numbers that should be implemented
  * inside of any companion object for a number
  */
trait BaseNumbers[T] {
  def zero : T
  def one : T
  def min : T
  def max : T
}

object UInt8 extends Factory[UInt8] with BaseNumbers[UInt8] {
  private case class UInt8Impl(underlying: Short) extends UInt8 {
    require(isValid(underlying), "Invalid range for a UInt8, got: " + underlying)
  }
  lazy val zero = UInt8(0.toShort)
  lazy val one = UInt8(1.toShort)

  lazy val min = zero
  lazy val max = UInt8(255.toShort)

  def apply(short: Short): UInt8 = UInt8Impl(short)

  def apply(byte: Byte): UInt8 = toUInt8(byte)

  def isValid(short: Short): Boolean = short >= 0  && short < 256

  override def fromBytes(bytes: Seq[Byte]): UInt8 = {
    val individualByteValues = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateUnsignedNumberFromByte(index, byte)
    UInt8(individualByteValues.sum.toShort)
  }

  def toUInt8(byte: Byte): UInt8 = {
    if ((byte & 0x80) == 0x80) {
      val r = (byte & 0x7f) + NumberUtil.pow2(7)
      UInt8(r.toShort)
    } else UInt8(byte.toShort)
  }

  def toByte(uInt8: UInt8): Byte = uInt8.underlying.toByte

  def toBytes(us: Seq[UInt8]): Seq[Byte] = us.map(toByte(_))

  def toUInt8s(bytes: Seq[Byte]): Seq[UInt8] = bytes.map(toUInt8(_))
}

object UInt32 extends Factory[UInt32] with BaseNumbers[UInt32] {
  private case class UInt32Impl(underlying : Long) extends UInt32 {
    require(underlying >= 0, "We cannot have a negative number in an unsigned number, got: " + underlying)
    require(underlying  <= 4294967295L, "We cannot have a number larger than 2^32 -1 in UInt32, got: " + underlying)
  }

  lazy val zero = UInt32(0)
  lazy val one = UInt32(1)

  lazy val min = zero
  lazy val max = UInt32(4294967295L)

  override def fromBytes(bytes : Seq[Byte]): UInt32 = {

    val individualByteValues = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateUnsignedNumberFromByte(index, byte)
    UInt32Impl(individualByteValues.sum.toLong)
  }

  def apply(long : Long): UInt32 = UInt32Impl(long)

}



object UInt64 extends Factory[UInt64] with BaseNumbers[UInt64] {
  private case class UInt64Impl(underlying : BigInt) extends UInt64 {
    require(underlying >= 0, "We cannot have a negative number in an unsigned number: " + underlying)
    require(underlying <= BigInt("18446744073709551615"), "We cannot have a number larger than 2^64 -1 in UInt64, got: " + underlying)
  }

  lazy val zero = UInt64(BigInt(0))
  lazy val one = UInt64(BigInt(1))

  lazy val min = zero
  lazy val max = fromBytes(Seq(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
    0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): UInt64 = {
    val individualByteValues : Seq[BigInt] = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateUnsignedNumberFromByte(index, byte)
    UInt64Impl(individualByteValues.sum)
  }

  def apply(num : BigInt): UInt64 = UInt64Impl(num)

}


object Int32 extends Factory[Int32] with BaseNumbers[Int32] {
  private case class Int32Impl(underlying : Int) extends Int32

  lazy val zero = Int32(0)
  lazy val one = Int32(1)

  lazy val min = fromBytes(Seq(0x80.toByte, 0.toByte, 0.toByte, 0.toByte))
  lazy val max = fromBytes(Seq(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): Int32 =  {
    require(bytes.size <= 4, "We cannot have an Int32 be larger than 4 bytes")
    Int32(BigInt(bytes.toArray).toInt)
  }

  def apply(int : Int): Int32 = Int32Impl(int)
}


object Int64 extends Factory[Int64] with BaseNumbers[Int64] {
  private case class Int64Impl(underlying : Long) extends Int64

  lazy val zero = Int64(0)
  lazy val one = Int64(1)

  lazy val min = fromBytes(Seq(0x80.toByte,0.toByte, 0.toByte, 0.toByte,0.toByte, 0.toByte, 0.toByte, 0.toByte))
  lazy val max = fromBytes(Seq(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
    0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): Int64 = {
    require(bytes.size <= 8, "We cannot have an Int64 be larger than 8 bytes")
    Int64(BigInt(bytes.toArray).toLong)
  }

  def apply(long : Long): Int64 = Int64Impl(long)
}
