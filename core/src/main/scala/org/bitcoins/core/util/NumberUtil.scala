package org.bitcoins.core.util

import java.math.BigInteger

import org.bitcoins.core.number._
import scodec.bits.ByteVector

import scala.math.BigInt
import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 2/8/16.
 */
trait NumberUtil extends BitcoinSLogger {

  private def parseLong(hex: String): Long = java.lang.Long.parseLong(hex, 16)

  private def parseLong(bytes: ByteVector): Long = parseLong(BitcoinSUtil.encodeHex(bytes))

  private def parseLong(byte: Byte): Long = parseLong(ByteVector.fromByte(byte))

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
  def convert[To <: Number[To]](data: Vector[UInt8], from: UInt32, to: UInt32, pad: Boolean, f: UInt8 => To): Try[Vector[To]] = {
    var acc: UInt32 = UInt32.zero
    var bits: UInt32 = UInt32.zero
    var ret: Vector[To] = Vector.empty
    val maxv: UInt32 = (UInt32.one << to) - UInt32.one
    val eight = UInt32(8)
    val fromU8 = UInt8(from.toLong.toShort)
    if (from > eight || to > eight) {
      Failure(new IllegalArgumentException("Can't have convert bits 'from' or 'to' parameter greater than 8"))
    } else {
      data.map { h: UInt8 =>
        if ((h >> fromU8.toInt) != UInt8.zero) {
          Failure(new IllegalArgumentException("Invalid input for bech32: " + h))
        } else {
          acc = (acc << from) | UInt32(h.toLong)
          bits = bits + from
          while (bits >= to) {
            bits = bits - to
            val newBase = UInt8((((acc >> bits) & maxv).toInt.toShort))
            val r: To = f(newBase)
            ret = ret :+ r
          }
        }
      }

      if (pad) {
        if (bits > UInt32.zero) {
          val r: Long = ((acc << (to - bits) & maxv)).toLong
          ret = ret ++ Seq(f(UInt8(r.toShort)))
        }
      } else if (bits >= from || ((acc << (to - bits)) & maxv) != UInt8.zero) {
        Failure(new IllegalArgumentException("Invalid padding in encoding"))
      }
      Success(ret)
    }
  }

  def convertBytes[T <: Number[T]](data: ByteVector, from: UInt32, to: UInt32, pad: Boolean, f: Byte => T): Try[Vector[T]] = {
    val wrapperF: UInt8 => T = { u8: UInt8 =>
      f(UInt8.toByte(u8))
    }
    convert[T](UInt8.toUInt8s(data), from, to, pad, wrapperF)
  }

  def convertUInt8sToUInt5s(u8s: Vector[UInt8]): Vector[UInt5] = {
    val f = { u8: UInt8 => UInt5.fromByte(UInt8.toByte(u8)) }
    val u8sTry = NumberUtil.convert[UInt5](u8s, UInt32(8), UInt32(5), true, f)
    u8sTry.get

  }

  def convertUInt5sToUInt8(u5s: Vector[UInt5]): Vector[UInt8] = {
    val u8s = u5s.map(_.toUInt8)
    val u8sTry = NumberUtil.convert[UInt8](u8s, UInt32(5), UInt32(8), false, { u8: UInt8 => u8 })
    //should always be able to convert from uint5 => uint8
    u8sTry.get
  }
}

object NumberUtil extends NumberUtil
