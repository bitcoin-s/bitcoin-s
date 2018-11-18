package org.bitcoins.core.util

import java.math.BigInteger

import org.bitcoins.core.number.{ UInt32, UInt8 }
import scodec.bits.ByteVector

import scala.math.BigInt
import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 2/8/16.
 */
trait NumberUtil extends BitcoinSLogger {

  /** Takes 2^^num. */
  def pow2(exponent: Int): BigInt = {
    require(exponent < 64, "We cannot have anything larger than 2^64 - 1 in a long, you tried to do 2^" + exponent)
    BigInt(1) << exponent
  }

  /** Converts a sequence of bytes to a **big endian** unsigned integer */
  def toUnsignedInt(bytes: ByteVector): BigInt = {
    BigInt(new BigInteger(1, bytes.toArray))
  }

  /** Takes a hex string and parses it to a [[BigInt]]. */
  def toBigInt(hex: String): BigInt = toBigInt(BitcoinSUtil.decodeHex(hex))

  /** Converts a sequence of bytes to twos complement signed number. */
  def toBigInt(bytes: ByteVector): BigInt = {
    //BigInt interprets the number as an unsigned number then applies the given
    //sign in front of that number, therefore if we have a negative number we need to invert it
    //since twos complement is an inverted number representation for negative numbers
    //see [[https://en.wikipedia.org/wiki/Two%27s_complement]]
    if (bytes.isEmpty) BigInt(0)
    //check if sign bit is set
    else if ((0x80.toByte & bytes.head) != 0) {
      val invertedBytes = bytes.tail.map(b => (b ^ 0xff.toByte).toByte)
      val firstByteInverted = (bytes.head ^ 0xff.toByte).toByte
      val num = firstByteInverted +: invertedBytes
      BigInt(-1, num.toArray) - 1
    } else {
      val firstBitOff = (0x7f & bytes.head).toByte
      val num = firstBitOff +: bytes.tail
      BigInt(num.toArray)
    }
  }

  /** Converts a sequence of [[Byte]] to a [[Int]]. */
  def toInt(bytes: ByteVector): Int = toBigInt(bytes).toInt

  /** Converts a hex string to a [[Int]]. */
  def toInt(hex: String): Int = toInt(BitcoinSUtil.decodeHex(hex))

  /** Converts a sequence of [[Byte]] to a [[Long]]. */
  def toLong(bytes: ByteVector): Long = toBigInt(bytes).toLong

  /** Converts a hex string to a [[Long]]. */
  def toLong(hex: String): Long = toLong(BitcoinSUtil.decodeHex(hex))

  /** Converts a sequence uint8 'from' base to 'to' base */
  def convertUInt8s(data: Seq[UInt8], from: UInt32, to: UInt32, pad: Boolean): Try[Seq[UInt8]] = {
    var acc: UInt32 = UInt32.zero
    var bits: UInt32 = UInt32.zero
    var ret: Seq[UInt8] = Nil
    val maxv: UInt32 = (UInt32.one << to) - UInt32.one
    val eight = UInt32(8)
    if (from > eight || to > eight) {
      Failure(new IllegalArgumentException("Can't have convert bits 'from' or 'to' parameter greater than 8"))
    } else {
      data.map { h =>
        if ((h >> UInt8(from.toLong.toShort)) != UInt8.zero) {
          Failure(new IllegalArgumentException("Invalid input for bech32: " + h))
        } else {
          acc = (acc << from) | UInt32(h.toLong)
          bits = bits + from
          while (bits >= to) {
            bits = bits - to
            val r: Seq[UInt8] = Seq(UInt8((((acc >> bits) & maxv).toInt.toShort)))
            ret = ret ++ r
          }
        }
      }

      if (pad) {
        if (bits > UInt32.zero) {
          val r: Long = ((acc << (to - bits) & maxv)).toLong
          ret = ret ++ Seq(UInt8(r.toShort))
        }
      } else if (bits >= from || ((acc << (to - bits)) & maxv) != UInt8.zero) {
        Failure(new IllegalArgumentException("Invalid padding in encoding"))
      }
      Success(ret)
    }
  }

  def convertBytes(data: ByteVector, from: UInt32, to: UInt32, pad: Boolean): Try[Seq[UInt8]] = {
    convertUInt8s(UInt8.toUInt8s(data), from, to, pad)
  }
}

object NumberUtil extends NumberUtil
