package org.bitcoins.core.number

import org.bitcoins.core.util.{BitcoinSLogger, Factory, NumberUtil}

/**
  * Created by chris on 6/4/16.
  */
sealed trait Number extends BitcoinSLogger {
  type A
  def underlying : A
  def + (num : Number): Number
  def - (num : A) : A = ???
  def * (num : A) : A = ???
}

sealed trait SignedNumber extends Number

sealed trait UnsignedNumber extends Number

sealed trait UInt32 extends UnsignedNumber {
  override type A = Long

  def + (num : Number): Number = num match {
    case uInt32 : UInt32 => UInt64(underlying + uInt32.underlying)
    case uInt64 : UInt64 => UInt64(uInt64.underlying + underlying)
    case int32 : Int32 => ???
    case int64 : Int64 => ???
  }
}

sealed trait UInt64 extends UnsignedNumber {
  override type A = BigInt
  override def + (num : Number) = ???
}

sealed trait Int32 extends SignedNumber {
  override type A = Int
  override def + (num : Number) = ???
}

sealed trait Int64 extends SignedNumber {
  override type A = Long
  override def + (num : Number) = ???
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


trait BaseNumbers {
  def zero : Number
  def one : Number
  def min : Number
  def max : Number
}

object UInt32 extends Factory[UInt32] with BitcoinSLogger with BaseNumbers {
  private case class UInt32Impl(underlying : Long) extends UInt32

  lazy val zero = fromBytes(Seq(0.toByte))
  lazy val one = fromBytes(Seq(1.toByte))

  lazy val min = zero
  lazy val max = fromBytes(Seq(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): UInt32 = {
    require(bytes.size <= 4, "We cannot have a UInt32 be larger than 4 bytes")
    val individualByteValues = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateNumberFromByte(index, byte)
    UInt32Impl(individualByteValues.sum.toLong)
  }

}



object UInt64 extends Factory[UInt64] with BitcoinSLogger with BaseNumbers {
  private case class UInt64Impl(underlying : BigInt) extends UInt64

  lazy val zero = UInt32.zero
  lazy val one = UInt32.one

  lazy val min = zero
  lazy val max = fromBytes(Seq(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
    0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): UInt64 = {
    require(bytes.size <= 8, "We cannot have a UInt64 larger than 8 bytes")
    val individualByteValues : Seq[BigInt] = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateNumberFromByte(index, byte)
    logger.debug("Individual bytes values: " + individualByteValues)
    UInt64Impl(individualByteValues.sum)
  }

  def apply(num : BigInt): UInt64 = UInt64Impl(num)
}


object Int32 extends Factory[Int32] with BaseNumbers {
  private case class Int32Impl(underlying : Int) extends Int32

  lazy val zero = fromBytes(Seq(0.toByte))
  lazy val one = fromBytes(Seq(1.toByte))

  lazy val min = fromBytes(Seq(0x80.toByte, 0.toByte, 0.toByte, 0.toByte))
  lazy val max = fromBytes(Seq(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))

  override def fromBytes(bytes : Seq[Byte]): Int32 =  {
    require(bytes.size <= 4, "We cannot have an Int32 be larger than 4 bytes")
    Int32Impl(BigInt(bytes.toArray).toInt)
  }
}


object Int64 extends Factory[Int64] with BaseNumbers {
  private case class Int64Impl(underlying : Long) extends Int64

  lazy val zero = Int32.zero
  lazy val one = Int32.one

  lazy val min = fromBytes(Seq(0x80.toByte,0.toByte, 0.toByte, 0.toByte,0.toByte, 0.toByte, 0.toByte, 0.toByte))
  lazy val max = fromBytes(Seq(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
    0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
  override def fromBytes(bytes : Seq[Byte]): Int64 = {
    require(bytes.size <= 8, "We cannot have an Int64 be larger than 8 bytes")
    Int64Impl(BigInt(bytes.toArray).toLong)
  }
}
