package org.bitcoins.core.number

import org.bitcoins.core.util.{BitcoinSLogger, Factory, NumberUtil}

/**
  * Created by chris on 6/4/16.
  */
sealed trait Number extends BitcoinSLogger {
  type A
  def underlying : A
  def + (num : Number): Number = ???
  def - (num : Number) : Number = ???
  def * (num : Number) : Number = ???


  
}

sealed trait SignedNumber extends Number

sealed trait UnsignedNumber extends Number

sealed trait UInt32 extends UnsignedNumber {
  override type A = Long
}

sealed trait UInt64 extends UnsignedNumber {
  override type A = BigInt
}

sealed trait Int32 extends SignedNumber {
  override type A = Int
}

sealed trait Int64 extends SignedNumber {
  override type A = Long
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



object UInt32 extends Factory[UInt32] with BitcoinSLogger {
  private case class UInt32Impl(underlying : Long) extends UInt32

  override def fromBytes(bytes : Seq[Byte]): UInt32 = {
    require(bytes.size <= 4, "We cannot have a UInt32 be larger than 4 bytes")
    val individualByteValues = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateNumberFromByte(index, byte)
    UInt32Impl(individualByteValues.sum.toLong)
  }

}



object UInt64 extends Factory[UInt64] with BitcoinSLogger {
  private case class UInt64Impl(underlying : BigInt) extends UInt64

  override def fromBytes(bytes : Seq[Byte]): UInt64 = {
    require(bytes.size <= 8, "We cannot have a UInt64 larger than 8 bytes")
    val individualByteValues : Seq[BigInt] = for {
      (byte,index) <- bytes.reverse.zipWithIndex
    } yield NumberUtil.calculateNumberFromByte(index, byte)
    logger.debug("Individual bytes values: " + individualByteValues)
    UInt64Impl(individualByteValues.sum)
  }
}


object Int32 extends Factory[Int32] {
  private case class Int32Impl(underlying : Int) extends Int32

  override def fromBytes(bytes : Seq[Byte]): Int32 =  {
    require(bytes.size <= 4, "We cannot have an Int32 be larger than 4 bytes")
    Int32Impl(BigInt(bytes.toArray).toInt)
  }
}


object Int64 extends Factory[Int64] {
  private case class Int64Impl(underlying : Long) extends Int64

  override def fromBytes(bytes : Seq[Byte]): Int64 = Int64Impl(BigInt(bytes.toArray).toLong)
}
