package org.bitcoins.core.number

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, Factory, NumberUtil}

import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 6/4/16.
  */

/**
  * A number can either be a signed or an unsigned number
  */
sealed trait Number extends NetworkElement with BitcoinSLogger {
  type A
  def underlying : A
}

/**
  * Represents a signed number in our number system
  * Instances of this are [[Int32]] or [[Int64]]
  */
sealed trait SignedNumber extends Number {
  protected def packageInSmallestType(long : Long) : Try[SignedNumber] = {
    if (long >= Int32.min.underlying && long <= Int32.max.underlying) Success(Int32(long.toInt))
    else if (long >= Int64.min.underlying && long <= Int64.max.underlying) Success(Int64(long))
    else Failure(new RuntimeException("The number: " + long + " is outside the range for valid signed numbers (-2^32 to 2^32-1 "))
  }

  protected def checkResult(result : Try[SignedNumber]) : SignedNumber = result match {
    case Success(number) => number
    case Failure(exception) => throw exception
  }
}

/**
  * Represents an unsigned number in our number system
  * Instances of this are [[UInt32]] or [[UInt64]]
  */
sealed trait UnsignedNumber extends Number {
  /**
    * Takes in a BigInt and packages it in the smallest [[UnsignedNumber]]
    * else it returns an exception if it cannot fit in a [[UInt64]]
    * @param bigInt
    * @return
    */
  protected def packageInSmallestType(bigInt : BigInt) : Try[UnsignedNumber] = {
    logger.debug("BigInt: "+ bigInt)
    if (bigInt <= UInt32.max.underlying) Success(UInt32(bigInt.toLong))
    else if (bigInt <= UInt64.max.underlying) Success(UInt64(bigInt))
    else Failure(new RuntimeException("Buffer overflow with two UnsignedIntegers"))
  }

  /**
    * Checks the result of the arithmetic operation to see if an error occurred
    * if an error does occur throw it, else return the [[UnsignedNumber]]
    * @param result the try type wrapping the result of the arithmetic operation
    * @return the result of the unsigned number operation
    */
  protected def checkResult(result : Try[UnsignedNumber]): UnsignedNumber = result match {
    case Success(number) => number
    case Failure(exception) => throw exception
  }
}

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


/**
  * Represents a uint32_t in C
  */
sealed trait UInt32 extends UnsignedNumber with NumberOperations[UnsignedNumber] {
  override type A = Long

  def + (num : UnsignedNumber): UnsignedNumber = num match {
    case uInt32 : UInt32 =>
      val n : BigInt = BigInt(underlying) + uInt32.underlying
      if (n == n.toLong) UInt32(n.toLong)
      else UInt64(n)
    case uInt64 : UInt64 => UInt64(uInt64.underlying + underlying)
  }

  def - (num : UnsignedNumber): UnsignedNumber = num match {
    case uInt32 : UInt32 =>
      val result = underlying - uInt32.underlying
      if (result < 0) throw new RuntimeException("Subtraction cannot lead to a negative number in UInt32")
      else UInt32(result)
    case uInt64 : UInt64 =>
      val result = underlying - uInt64.underlying
      if (result < 0) throw new RuntimeException("Subtracting UInt64 from a UInt32 cannot lead to a negative number")
      else {
        require(result == result.toLong, "Subtracting a UInt64 from a UInt32 should give us a UInt32")
        UInt32(result.toLong)
      }
  }

  override def * (num : UnsignedNumber): UnsignedNumber = num match {
    case uInt32 : UInt32 =>
      val product = BigInt(underlying) * BigInt(uInt32.underlying)
      val result = packageInSmallestType(product)
      checkResult(result)
    case uInt64 : UInt64 =>
      val result = packageInSmallestType(uInt64.underlying * underlying)
      checkResult(result)
  }

  def > (num : UnsignedNumber): Boolean = num match {
    case uInt32 : UInt32 => underlying > uInt32.underlying
    case uInt64 : UInt64 => underlying > uInt64.underlying
  }

  def >= (num : UnsignedNumber): Boolean = num match {
    case uInt32 : UInt32 => underlying >= uInt32.underlying
    case uInt64 : UInt64 => underlying >= uInt64.underlying
  }

  def < (num : UnsignedNumber): Boolean = num match {
    case uInt32 : UInt32 => underlying < uInt32.underlying
    case uInt64 : UInt64 => underlying < uInt64.underlying
  }

  def <= (num : UnsignedNumber): Boolean = num match {
    case uInt32 : UInt32 => underlying <= uInt32.underlying
    case uInt64 : UInt64 => underlying <= uInt64.underlying
  }

}

/**
  * Represents a uint64_t in C
  */
sealed trait UInt64 extends UnsignedNumber with NumberOperations[UnsignedNumber] {
  override type A = BigInt
  def + (num : UnsignedNumber): UnsignedNumber = {
    val result = num match {
      case uInt32 : UInt32 => underlying + uInt32.underlying
      case uInt64 : UInt64 => underlying + uInt64.underlying
    }
    if (result > UInt64.max.underlying) throw new RuntimeException("The sum of " + this + " and " + num +
      "is larger than UInt64.max")
    else UInt64(result)
  }

  def - (num : UnsignedNumber): UnsignedNumber = {
    val result = num match {
      case uInt32 : UInt32 =>underlying - uInt32.underlying
      case uInt64 : UInt64 => underlying - uInt64.underlying
    }
    if (result < 0) throw new RuntimeException("Cannot have a negative unsigned number, " +
      "got one subtracting: " + this + " and " + num)
    else UInt64(result)
  }

  override def * (num : UnsignedNumber): UnsignedNumber = {
    val result = num match {
      case uInt32 : UInt32 => underlying * uInt32.underlying
      case uInt64 : UInt64 => underlying * uInt64.underlying
    }
    if (result > UInt64.max.underlying) throw new RuntimeException("The product of " + this +
      " and " + num + " was too large for UInt64")
    else UInt64(result)
  }

  def > (num : UnsignedNumber): Boolean = num match {
    case uInt32: UInt32 => underlying > uInt32.underlying
    case uInt64: UInt64 => underlying > uInt64.underlying
  }
  def >= (num : UnsignedNumber): Boolean = num match {
    case uInt32 : UInt32 => underlying >= uInt32.underlying
    case uInt64 : UInt64 => underlying >= uInt64.underlying
  }
  def < (num : UnsignedNumber): Boolean = num match {
    case uInt32 : UInt32 => underlying < uInt32.underlying
    case uInt64 : UInt64 => underlying < uInt64.underlying
  }
  def <= (num : UnsignedNumber): Boolean = num match {
    case uInt32: UInt32 => underlying <= uInt32.underlying
    case uInt64: UInt64 => underlying <= uInt64.underlying
  }

}

/**
  * Represents a int32_t in C
  */
sealed trait Int32 extends SignedNumber with NumberOperations[SignedNumber] {
  override type A = Int
  override def + (num : SignedNumber) = {
    val sum = num match {
      case int32 : Int32 => underlying + int32.underlying
      case int64 : Int64 => underlying + int64.underlying
    }
    val result = packageInSmallestType(sum)
    checkResult(result)
  }
  override def - (num : SignedNumber) = {
    val difference = num match {
      case int32 : Int32 => underlying - int32.underlying
      case int64 : Int64 => underlying - int64.underlying
    }
    val result = packageInSmallestType(difference)
    checkResult(result)
  }

  override def *(num : SignedNumber) = {
    val product = num match {
      case int32 : Int32 => underlying * int32.underlying
      case int64 : Int64 => underlying * int64.underlying
    }
    val result = packageInSmallestType(product)
    checkResult(result)
  }
  override def > (num : SignedNumber): Boolean = num match {
    case int32 : Int32 => underlying > int32.underlying
    case int64 : Int64 => underlying > int64.underlying
  }
  override def >= (num : SignedNumber): Boolean = num match {
    case int32 : Int32 => underlying >= int32.underlying
    case int64 : Int64 => underlying >= int64.underlying
  }
  override def < (num : SignedNumber): Boolean = num match {
    case int32 : Int32 => underlying < int32.underlying
    case int64 : Int64 => underlying < int64.underlying
  }
  override def <= (num : SignedNumber): Boolean = num match {
    case int32 : Int32 => underlying <= int32.underlying
    case int64 : Int64 => underlying <= int64.underlying
  }
}

/**
  * Represents a int64_t in C
  */
sealed trait Int64 extends SignedNumber with NumberOperations[SignedNumber] {
  override type A = Long
  override def + (num : SignedNumber) = {
    val sum = num match {
      case int32 : Int32 => underlying + int32.underlying
      case int64 : Int64 => underlying + int64.underlying
    }
    val result = packageInSmallestType(sum)
    checkResult(result)
  }
  override def - (num : SignedNumber) = {
    val difference = num match {
      case int32 : Int32 => underlying - int32.underlying
      case int64 : Int64 => underlying - int64.underlying
    }
    val result = packageInSmallestType(difference)
    checkResult(result)
  }
  override def * (num : SignedNumber) = {
    val product = num match {
      case int32 : Int32 => underlying * int32.underlying
      case int64 : Int64 => underlying * int64.underlying
    }
    val result = packageInSmallestType(product)
    checkResult(result)
  }

  override def > (num : SignedNumber): Boolean = num match {
    case int32 : Int32 => underlying > int32.underlying
    case int64 : Int64 => underlying > int64.underlying
  }
  override def >= (num : SignedNumber): Boolean = num match {
    case int32 : Int32 => underlying >= int32.underlying
    case int64 : Int64 => underlying >= int64.underlying
  }
  override def < (num : SignedNumber): Boolean = num match {
    case int32 : Int32 => underlying < int32.underlying
    case int64 : Int64 => underlying < int64.underlying
  }
  override def <= (num : SignedNumber): Boolean = num match {
    case int32 : Int32 => underlying <= int32.underlying
    case int64 : Int64 => underlying <= int64.underlying
  }
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
  * inside of any companion object for a [[Number]]
  */
trait BaseNumbers[T <: Number] {
  def zero : T
  def one : T
  def min : T
  def max : T
}

object UInt32 extends Factory[UInt32] with BitcoinSLogger with BaseNumbers[UInt32] {
  private case class UInt32Impl(underlying : Long, hex : String) extends UInt32 {
    require(underlying >= 0, "We cannot have a negative number in an unsigned number, got: " + underlying)
    require(underlying <= 4294967295L, "We cannot have a number larger than 2^32 -1 in UInt32, got: " + underlying)
  }

  lazy val zero = UInt32(0)
  lazy val one = UInt32(1)

  lazy val min = zero
  lazy val max = UInt32(4294967295L)

  override def fromBytes(bytes : Seq[Byte]): UInt32 = {

    val individualByteValues = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateNumberFromByte(index, byte)
    UInt32Impl(individualByteValues.sum.toLong, BitcoinSUtil.encodeHex(bytes))
  }

  def apply(long : Long) : UInt32 = UInt32Impl(long, BitcoinSUtil.encodeHex(long))

}



object UInt64 extends Factory[UInt64] with BitcoinSLogger with BaseNumbers[UInt64] {
  private case class UInt64Impl(underlying : BigInt, hex : String) extends UInt64 {
    require(underlying >= 0, "We cannot have a negative number in an unsigned number: " + underlying)
    require(underlying <= BigInt("18446744073709551615"), "We cannot have a number larger than 2^64 -1 in UInt32, got: " + underlying)
  }

  lazy val zero = UInt64(0)
  lazy val one = UInt64(1)

  lazy val min = zero
  lazy val max = fromBytes(Seq(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
    0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): UInt64 = {
    val individualByteValues : Seq[BigInt] = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateNumberFromByte(index, byte)
    logger.debug("Individual bytes values: " + individualByteValues)
    UInt64Impl(individualByteValues.sum, BitcoinSUtil.encodeHex(bytes))
  }

  def apply(num : BigInt): UInt64 = {
    if (num >= (BigInt(1) << 63)) {
      //since Scala uses twos complement, it will add a padding byte
      //if the number is > 2^63. We can remove this since we want to
      //represent this number as unsigned
      val bytes = num.toByteArray.tail
      UInt64Impl(num, BitcoinSUtil.encodeHex(bytes))
    } else UInt64Impl(num, BitcoinSUtil.encodeHex(num))
  }

}


object Int32 extends Factory[Int32] with BaseNumbers[Int32] {
  private case class Int32Impl(underlying : Int, hex : String) extends Int32

  lazy val zero = Int32(0)
  lazy val one = Int32(1)

  lazy val min = fromBytes(Seq(0x80.toByte, 0.toByte, 0.toByte, 0.toByte))
  lazy val max = fromBytes(Seq(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): Int32 =  {
    require(bytes.size <= 4, "We cannot have an Int32 be larger than 4 bytes")
    Int32Impl(BigInt(bytes.toArray).toInt, BitcoinSUtil.encodeHex(bytes))
  }

  def apply(int : Int): Int32 = Int32Impl(int, BitcoinSUtil.encodeHex(int))
}


object Int64 extends Factory[Int64] with BaseNumbers[Int64] {
  private case class Int64Impl(underlying : Long, hex : String) extends Int64

  lazy val zero = Int64(0)
  lazy val one = Int64(1)

  lazy val min = fromBytes(Seq(0x80.toByte,0.toByte, 0.toByte, 0.toByte,0.toByte, 0.toByte, 0.toByte, 0.toByte))
  lazy val max = fromBytes(Seq(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
    0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): Int64 = {
    require(bytes.size <= 8, "We cannot have an Int64 be larger than 8 bytes")
    Int64Impl(BigInt(bytes.toArray).toLong, BitcoinSUtil.encodeHex(bytes))
  }

  def apply(long : Long): Int64 = Int64Impl(long, BitcoinSUtil.encodeHex(long))
}
