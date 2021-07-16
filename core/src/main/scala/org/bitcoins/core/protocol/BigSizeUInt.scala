package org.bitcoins.core.protocol

import org.bitcoins.core.number.UInt64
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

case class BigSizeUInt(num: UInt64) extends NetworkElement {
  override val byteSize: Long = BigSizeUInt.calcSizeForNum(num)

  override def bytes: ByteVector = {
    byteSize match {
      case 1 => num.bytes.takeRight(1)
      case 3 => 0xfd.toByte +: num.bytes.takeRight(2)
      case 5 => 0xfe.toByte +: num.bytes.takeRight(4)
      case _ => 0xff.toByte +: num.bytes
    }
  }

  def toBigInt: BigInt = num.toBigInt

  def toLong: Long = num.toLong

  def toInt: Int = {
    val l = toLong
    require(Int.MinValue <= l && l <= Int.MaxValue,
            "Cannot convert BigSizeUInt toInt, got: " + this)
    l.toInt
  }

  override def toString: String = s"BigSizeUInt(${num.toLong})"
}

object BigSizeUInt extends Factory[BigSizeUInt] {

  def calcSizeForNum(num: UInt64): Int = {
    if (num.toBigInt < 0xfd) { // can be represented with one byte
      1
    } else if (num.toBigInt < 0x10000) { // can be represented with two bytes
      3
    } else if (num.toBigInt < 0x100000000L) { // can be represented with 4 bytes
      5
    } else {
      9
    }
  }

  def apply(num: Long): BigSizeUInt = {
    BigSizeUInt(UInt64(num))
  }

  def apply(num: BigInt): BigSizeUInt = {
    BigSizeUInt(UInt64(num))
  }

  override def fromBytes(bytes: ByteVector): BigSizeUInt = {
    require(bytes.nonEmpty, "Cannot parse a BigSizeUInt from empty byte vector")

    val prefixNum = UInt64(bytes.take(1)).toInt

    val (bigSizeUInt, expectedSize) = if (prefixNum < 253) { // 8 bit number
      (BigSizeUInt(prefixNum), 1)
    } else if (prefixNum == 253) { // 16 bit number
      (BigSizeUInt(UInt64(bytes.slice(1, 3))), 3)
    } else if (prefixNum == 254) { // 32 bit number
      (BigSizeUInt(UInt64(bytes.slice(1, 5))), 5)
    } else { // 64 bit number
      (BigSizeUInt(UInt64(bytes.slice(1, 9))), 9)
    }

    require(
      bigSizeUInt.byteSize == expectedSize,
      s"Length prefix $prefixNum did not match bytes ${bigSizeUInt.bytes.tail}")

    bigSizeUInt
  }

  def calcFor(bytes: ByteVector): BigSizeUInt = {
    BigSizeUInt(bytes.length)
  }
}
