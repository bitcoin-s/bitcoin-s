package org.bitcoins.core.protocol

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.script.constant.ScriptNumberUtil
import org.bitcoins.core.util.{BitcoinSUtil, Factory}

/**
 * Created by chris on 7/14/15.
 */

/**
  * Compact sized unsigned integer as described in:
  * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
  */
trait CompactSizeUInt {

  /** The number parsed from VarInt. */
  def num: UInt64

  /** The length of the VarInt in bytes. */
  def size: Long

  def hex = size match {
    case 1 => BitcoinSUtil.flipEndianness(num.hex.slice(14,16))
    case 3 => "fd" + BitcoinSUtil.flipEndianness(num.hex.slice(12,16))
    case 5 => "fe" + BitcoinSUtil.flipEndianness(num.hex.slice(8,16))
    case _ => "ff" + BitcoinSUtil.flipEndianness(num.hex)
  }

  def bytes: Seq[Byte] = BitcoinSUtil.decodeHex(hex)
}

object CompactSizeUInt extends Factory[CompactSizeUInt] {
  private sealed case class CompactSizeUIntImpl(num : UInt64, size : Long) extends CompactSizeUInt

  override def fromBytes(bytes: Seq[Byte]): CompactSizeUInt = {
    parseCompactSizeUInt(bytes)
  }

  def apply(num : UInt64, size : Long) : CompactSizeUInt = {
    CompactSizeUIntImpl(num,size)
  }

  def apply(num : UInt64): CompactSizeUInt = {
    val size = calcSizeForNum(num)
    CompactSizeUInt(num,size)
  }

  private def calcSizeForNum(num : UInt64) : Int = {
    if (num.underlying <= 252) 1
    // can be represented with two bytes
    else if (num.underlying <= 65535) 3
    //can be represented with 4 bytes
    else if (num.underlying <= UInt32.max.underlying) 5
    else 9
  }
  /** This function is responsible for calculating what the compact size unsigned integer is for a
    * sequence of bytes
    * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers. */
  def calculateCompactSizeUInt(bytes : Seq[Byte]) : CompactSizeUInt = {
    //means we can represent the number with a single byte
    if (bytes.size <= 252) CompactSizeUInt(UInt64(bytes.size),1)
    // can be represented with two bytes
    else if (bytes.size <= 65535) CompactSizeUInt(UInt64(bytes.size),3)
    //can be represented with 4 bytes
    else if (bytes.size <= UInt32.max.underlying) CompactSizeUInt(UInt64(bytes.size),5)
    else CompactSizeUInt(UInt64(bytes.size),9)
  }

  def calc(bytes: Seq[Byte]): CompactSizeUInt = calculateCompactSizeUInt(bytes)

  /** Responsible for calculating what the [[CompactSizeUInt]] is for this hex string. */
  def calculateCompactSizeUInt(hex : String) : CompactSizeUInt = calculateCompactSizeUInt(BitcoinSUtil.decodeHex(hex))

  /** Parses a VarInt from a string of hex characters
    * [[https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers]] */
  def parseCompactSizeUInt(hex : String) : CompactSizeUInt = parseCompactSizeUInt(BitcoinSUtil.decodeHex(hex))

  /** Parses a [[CompactSizeUInt]] from a sequence of bytes
    * [[https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers]] */
  def parseCompactSizeUInt(bytes : Seq[Byte]) : CompactSizeUInt = {
    require(bytes.nonEmpty, "Cannot parse a VarInt if the byte array is size 0")
    //8 bit number
    if (UInt64(Seq(bytes.head)).underlying < 253)
      CompactSizeUInt(UInt64(Seq(bytes.head)),1)
    //16 bit number
    else if (UInt64(Seq(bytes.head)).toInt == 253) CompactSizeUInt(UInt64(bytes.slice(1,3).reverse),3)
    //32 bit number
    else if (UInt64(Seq(bytes.head)).toInt == 254) CompactSizeUInt(UInt64(bytes.slice(1,5).reverse),5)
    //64 bit number
    else CompactSizeUInt(UInt64(bytes.slice(1,9).reverse),9)
  }

  def parse(bytes: Seq[Byte]): CompactSizeUInt = parseCompactSizeUInt(bytes)

  /** Returns the size of a VarInt in the number of bytes
    * https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer. */
  def parseCompactSizeUIntSize(byte : Byte) : Long = {
    //8 bit number
    if (parseLong(byte) < 253) 1
    //16 bit number
    else if (parseLong(byte) == 253) 3
    //32 bit number
    else if (parseLong(byte) == 254) 5
    //64 bit number
    else 9
  }

  /** Parses the [[CompactSizeUInt]] from a [[ScriptSignature]].
    * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers. */
  def parseCompactSizeUInt(script : ScriptSignature) : CompactSizeUInt = {
    if (script.bytes.size <=252 ) {
      CompactSizeUInt(UInt64(script.bytes.size),1)
    } else if (script.bytes.size <= 0xffff) {
      CompactSizeUInt(UInt64(script.bytes.size),3)
    } else if (script.bytes.size <= 0xffffffffL) {
      CompactSizeUInt(UInt64(script.bytes.size),5)
    }
    else CompactSizeUInt(UInt64(script.bytes.size),9)
  }

  private def parseLong(hex : String) : Long = java.lang.Long.parseLong(hex,16)

  private def parseLong(bytes : List[Byte]) : Long = parseLong(BitcoinSUtil.encodeHex(bytes))

  private def parseLong(byte : Byte) : Long = parseLong(List(byte))
}





