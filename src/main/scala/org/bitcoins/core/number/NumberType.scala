package org.bitcoins.core.number

import org.bitcoins.core.script.constant.ScriptNumberUtil
import org.bitcoins.core.util.{BitcoinSUtil, Factory, NumberUtil}

/**
  * Created by chris on 6/4/16.
  */
sealed trait Number {
  type A
  def underlying : A
  def + (num : Number): Number = ??? //executeBinaryArithmeticOperation(underlying,num.underlying,)
  def - (num : Number) : Number = ???
  def * (num : Number) : Number = ???

  private def performBinaryArithmeticOp(num1 : Number, num2 : Number, op : (Number,Number) => Number) : Number = (num1,num2) match {
    case (x : UInt32, y : UInt32) => ???
    case (x : UInt32, y : UInt64) => ???
  }
  
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



object UInt32 extends Factory[UInt32] {
  private case class UInt32Impl(underlying : Long) extends UInt32

  override def fromBytes(bytes : Seq[Byte]): UInt32 = ??? //UInt32Impl(toLong(bytes.toArray))
}



object UInt64 extends Factory[UInt64] {
  private case class UInt64Impl(underlying : BigInt) extends UInt64

  override def fromBytes(bytes : Seq[Byte]): UInt64 = UInt64Impl(BigInt(bytes.toArray))
}


object Int32 extends Factory[Int32] {
  private case class Int32Impl(underlying : Int) extends Int32

  override def fromBytes(bytes : Seq[Byte]): Int32 = ??? //Int32Impl(NumberUtil.toInt(bytes))
}


object Int64 extends Factory[Int64] {
  private case class Int64Impl(underlying : Long) extends Int64

  override def fromBytes(bytes : Seq[Byte]): Int64 = ??? ///Int64Impl(NumberUtil.toLong(bytes))
}
