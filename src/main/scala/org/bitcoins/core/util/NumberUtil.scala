package org.bitcoins.core.util

import scala.math.BigInt

/**
 * Created by chris on 2/8/16.
 */
trait NumberUtil extends BitcoinSLogger {

  private def parseLong(hex : String) : Long = java.lang.Long.parseLong(hex,16)

  private def parseLong(bytes : List[Byte]) : Long = parseLong(BitcoinSUtil.encodeHex(bytes))

  private def parseLong(byte : Byte) : Long = parseLong(List(byte))

  private def parseLong(bytes : Seq[Byte]) : Long = parseLong(bytes.toList)

  /** Takes 2^^num. */
  def pow2(exponent : Int) : BigInt = {
    require(exponent < 64, "We cannot have anything larger than 2^64 - 1 in a long, you tried to do 2^" + exponent)
    BigInt(1) << exponent
  }

  /**
    * Calculates the unsigned number for a byte
    * @param byteIndex this is used to tell what position this byte is out of a 4 byte integer
    *                     For instance, if byte was equal to 0x0001 and we were trying to calculate the unsigned int for
    *                     the following byte value Seq(0xf000, 0x0f00, 0x0001, 0x0000) we would have byteIndex 1
    * @param byte the byte which we need to calculate the unsigned integer for
    * @return the unsigned integer corresponding to the given byteIndex and byte
    */
  def calculateUnsignedNumberFromByte(byteIndex : Int, byte : Byte): BigInt = {
    val setBits : Seq[BigInt] = for {
      i <- 0 until 8
      bitIndex = i + (byteIndex * 8)
    } yield {
      //check if index i is set in the byte, if so we need to calculate 2 ^ bitIndex
      if ((pow2(i) & byte) != 0) pow2(bitIndex)
      else BigInt(0)
    }
    setBits.foldLeft(BigInt(0)){_ + _}
  }

  /** Takes a hex string and parses it to a [[BigInt]]. */
  def toBigInt(hex : String) : BigInt = toBigInt(BitcoinSUtil.decodeHex(hex))

  /** Converts a sequence of bytes to twos complement signed number. */
  def toBigInt(bytes : Seq[Byte]) : BigInt = {
    //BigInt interprets the number as an unsigned number then applies the given
    //sign in front of that number, therefore if we have a negative number we need to invert it
    //since twos complement is an inverted number representation for negative numbers
    //see [[https://en.wikipedia.org/wiki/Two%27s_complement]]
    if (bytes.isEmpty) BigInt(0)
    //check if sign bit is set
    else if ((0x80.toByte & bytes.head) !=0) {
      val invertedBytes = bytes.tail.map(b => (b ^ 0xff.toByte).toByte)
      val firstByteInverted = (bytes.head ^ 0xff.toByte).toByte
      val num = firstByteInverted +: invertedBytes
      BigInt(-1,num.toArray) - 1
    } else {
      val firstBitOff = (0x7f & bytes.head).toByte
      val num = firstBitOff +: bytes.tail
      BigInt(num.toArray)
    }
  }

  /** Converts a sequence of [[Byte]] to a [[Int]]. */
  def toInt(bytes : Seq[Byte]) : Int = toBigInt(bytes).toInt

  /** Converts a hex string to a [[Int]]. */
  def toInt(hex : String) : Int = toInt(BitcoinSUtil.decodeHex(hex))

  /** Converts a sequence of [[Byte]] to a [[Long]]. */
  def toLong(bytes : Seq[Byte]) : Long = toBigInt(bytes).toLong

  /** Converts a hex string to a [[Long]]. */
  def toLong(hex : String): Long = toLong(BitcoinSUtil.decodeHex(hex))

}

object NumberUtil extends NumberUtil
