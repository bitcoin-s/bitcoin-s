package org.bitcoins.core.protocol

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.crypto.{BytesUtil, Factory, NetworkElement}
import scodec.bits.ByteVector

/**
  * Compact sized unsigned integer, a Bitcoin-native data structure
  *
  * @see https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
  */
sealed abstract class CompactSizeUInt extends NetworkElement {

  /** The number parsed from VarInt. */
  def num: UInt64

  override def bytes: ByteVector = {
    byteSize match {
      case 1 => num.bytes.takeRight(1)
      case 3 => 0xfd.toByte +: num.bytes.takeRight(2).reverse
      case 5 => 0xfe.toByte +: num.bytes.takeRight(4).reverse
      case _ => 0xff.toByte +: num.bytes.reverse
    }
  }

  def toLong: Long = num.toLong

  def toInt: Int = {
    val l = toLong
    require(Int.MinValue <= l && l <= Int.MaxValue,
            "Cannot convert CompactSizeUInt toInt, got: " + this)
    l.toInt
  }

  override def toString(): String = s"CompactSizeUInt(${num.toLong})"
}

object CompactSizeUInt extends Factory[CompactSizeUInt] {
  private case class CompactSizeUIntImpl(
      num: UInt64,
      override val byteSize: Long)
      extends CompactSizeUInt

  val zero: CompactSizeUInt = CompactSizeUInt(UInt64.zero)
  override def fromBytes(bytes: ByteVector): CompactSizeUInt = {
    parseCompactSizeUInt(bytes)
  }

  def apply(num: UInt64, size: Int): CompactSizeUInt = {
    CompactSizeUIntImpl(num, size)
  }

  def apply(num: UInt64): CompactSizeUInt = {
    val size = calcSizeForNum(num)
    CompactSizeUInt(num, size)
  }

  private def calcSizeForNum(num: UInt64): Int = {
    if (num.toBigInt <= 252) 1
    // can be represented with two bytes
    else if (num.toBigInt <= 65535) 3
    //can be represented with 4 bytes
    else if (num.toBigInt <= UInt32.max.toBigInt) 5
    else 9
  }

  /**
    * This function is responsible for calculating what the compact size unsigned integer is for a
    * sequence of bytes
    * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers.
    */
  def calculateCompactSizeUInt(bytes: ByteVector): CompactSizeUInt = {
    //means we can represent the number with a single byte
    if (bytes.size <= 252) CompactSizeUInt(UInt64(bytes.size), 1)
    // can be represented with two bytes
    else if (bytes.size <= 65535) CompactSizeUInt(UInt64(bytes.size), 3)
    //can be represented with 4 bytes
    else if (bytes.size <= UInt32.max.toBigInt)
      CompactSizeUInt(UInt64(bytes.size), 5)
    else CompactSizeUInt(UInt64(bytes.size), 9)
  }

  def calc(bytes: ByteVector): CompactSizeUInt = calculateCompactSizeUInt(bytes)

  /** Responsible for calculating what the
    * [[org.bitcoins.core.protocol.CompactSizeUInt CompactSizeUInt]] is for this hex string. */
  def calculateCompactSizeUInt(hex: String): CompactSizeUInt =
    calculateCompactSizeUInt(BytesUtil.decodeHex(hex))

  /**
    * Parses a VarInt from a string of hex characters
    * [[https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers]]
    */
  def parseCompactSizeUInt(hex: String): CompactSizeUInt =
    parseCompactSizeUInt(BytesUtil.decodeHex(hex))

  /**
    * Parses a [[org.bitcoins.core.protocol.CompactSizeUInt CompactSizeUInt]] from a sequence of bytes
    * [[https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers]]
    */
  def parseCompactSizeUInt(bytes: ByteVector): CompactSizeUInt = {
    require(bytes.nonEmpty, "Cannot parse a VarInt if the byte array is size 0")
    val firstByte = UInt64(ByteVector(bytes.head))
    //8 bit number
    if (firstByte.toInt < 253)
      CompactSizeUInt(firstByte, 1)
    //16 bit number
    else if (firstByte.toInt == 253)
      CompactSizeUInt(UInt64(bytes.slice(1, 3).reverse), 3)
    //32 bit number
    else if (firstByte.toInt == 254)
      CompactSizeUInt(UInt64(bytes.slice(1, 5).reverse), 5)
    //64 bit number
    else CompactSizeUInt(UInt64(bytes.slice(1, 9).reverse), 9)
  }

  def parse(bytes: ByteVector): CompactSizeUInt = parseCompactSizeUInt(bytes)

  /**
    * Returns the size of a VarInt in the number of bytes
    * https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer.
    */
  def parseCompactSizeUIntSize(byte: Byte): Long = {
    //8 bit number
    if (parseLong(byte) < 253) 1
    //16 bit number
    else if (parseLong(byte) == 253) 3
    //32 bit number
    else if (parseLong(byte) == 254) 5
    //64 bit number
    else 9
  }

  /**
    * Parses the [[org.bitcoins.core.protocol.CompactSizeUInt CompactSizeUInt]] from a
    * [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]].
    * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers.
    */
  def parseCompactSizeUInt(script: ScriptSignature): CompactSizeUInt = {
    if (script.bytes.size <= 252) {
      CompactSizeUInt(UInt64(script.bytes.size), 1)
    } else if (script.bytes.size <= 0xffff) {
      CompactSizeUInt(UInt64(script.bytes.size), 3)
    } else if (script.bytes.size <= 0xFFFFFFFFL) {
      CompactSizeUInt(UInt64(script.bytes.size), 5)
    } else CompactSizeUInt(UInt64(script.bytes.size), 9)
  }

  private def parseLong(hex: String): Long = java.lang.Long.parseLong(hex, 16)

  private def parseLong(bytes: ByteVector): Long =
    parseLong(BytesUtil.encodeHex(bytes))

  private def parseLong(byte: Byte): Long = parseLong(ByteVector.fromByte(byte))
}
