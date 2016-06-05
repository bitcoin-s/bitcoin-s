package org.bitcoins.core.number

import org.bitcoins.core.util.{BitcoinSUtil, Factory, NumberUtil}

/**
  * Created by chris on 6/4/16.
  */
sealed trait Number {
  def + (num : Number) : Number = ???
  def - (num : Number) : Number = ???
  def * (num : Number) : Number = ???
}

sealed trait SignedNumber extends Number

object SignedNumber extends Factory[SignedNumber] {
  private case class SignedNumberImpl(underlying : BigInt) extends SignedNumber
  override def fromBytes(bytes : Seq[Byte]): SignedNumber = SignedNumberImpl(BigInt(bytes.toArray))
}

sealed trait UnsignedNumber extends Number

object UnsignedNumber extends Factory[UnsignedNumber] {
  private case class UnsignedNumberImpl(underlying : BigInt) extends UnsignedNumber

  override def fromBytes(bytes : Seq[Byte]): UnsignedNumber = UnsignedNumberImpl(BigInt(bytes.toArray))
}

sealed trait UInt32 extends UnsignedNumber {
  def underlying : Long
}

object UInt32 extends Factory[UInt32] {
  private case class UInt32Impl(underlying : Long) extends UInt32

  override def fromBytes(bytes : Seq[Byte]): UInt32 = UInt32Impl(NumberUtil.toLong(bytes.toArray))
}

sealed trait UInt64 extends UnsignedNumber {
  def underlying : BigInt
}

object UInt64 extends Factory[UInt64] {
  private case class UInt64Impl(underlying : BigInt) extends UInt64

  override def fromBytes(bytes : Seq[Byte]): UInt64 = UInt64Impl(BigInt(bytes.toArray))
}

sealed trait Int32 extends SignedNumber {
  def underlying : Int
}
object Int32 extends Factory[Int32] {
  private case class Int32Impl(underlying : Int) extends Int32

  override def fromBytes(bytes : Seq[Byte]): Int32 = Int32Impl(NumberUtil.toInt(bytes))
}

sealed trait Int64 extends SignedNumber {
  def underlying : Long
}
object Int64 extends Factory[Int64] {
  private case class Int64Impl(underlying : Long) extends Int64

  override def fromBytes(bytes : Seq[Byte]): Int64 = Int64Impl(NumberUtil.toLong(bytes))
}
