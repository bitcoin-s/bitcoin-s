package org.bitcoins.core.number

import org.bitcoins.core.script.constant.ScriptNumberUtil
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, Factory, NumberUtil}

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
    } yield calculateNumberFromByte(index, byte)
    logger.debug("Individual byte values: " + individualByteValues)
    UInt32Impl(individualByteValues.sum)
  }

  def pow2(num : Int) : Long = math.pow(2,num).toLong

  /**
    * Calculates the unsigned number for a byte
    * @param byteIndex this is used to tell what position this byte is out of a 4 byte integer
    *                     For instance, if byte was equal to 0x0001 and we were trying to calculate the unsigned int for
    *                     the following byte value Seq(0xf000, 0x0f00, 0x0001, 0x0000) we would have byteIndex 1
    * @param byte the byte which we need to calculate the unsigned integer for
    * @return the unsigned integer corresponding to the given byteIndex and byte
    */
  private def calculateNumberFromByte(byteIndex : Int, byte : Byte): Long = {
    val setBits = for {
      i <- 0 until 8
      bitIndex = i + (byteIndex * 8)
    } yield {
      //check if index i is set in the byte, if so we need to calculate 2 ^ bitIndex
      if ((pow2(i) & byte) != 0) {
        logger.debug("Bitindex: " + bitIndex)
        pow2(bitIndex)
      }
      else 0
    }
    logger.debug("Set bits: " + setBits)
    setBits.sum
  }
}



object UInt64 extends Factory[UInt64] {
  private case class UInt64Impl(underlying : BigInt) extends UInt64

  override def fromBytes(bytes : Seq[Byte]): UInt64 = {
    require(bytes.size <= 8, "We cannot have a UInt64 larger than 8 bytes")
    UInt64Impl(BigInt(bytes.toArray))
  }
}


object Int32 extends Factory[Int32] {
  private case class Int32Impl(underlying : Int) extends Int32

  override def fromBytes(bytes : Seq[Byte]): Int32 = Int32Impl(BigInt(bytes.toArray).toInt)
}


object Int64 extends Factory[Int64] {
  private case class Int64Impl(underlying : Long) extends Int64

  override def fromBytes(bytes : Seq[Byte]): Int64 = Int64Impl(BigInt(bytes.toArray).toLong)
}
