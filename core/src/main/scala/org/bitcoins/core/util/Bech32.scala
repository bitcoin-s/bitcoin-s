package org.bitcoins.core.util

import org.bitcoins.core.number.{ UInt32, UInt5, UInt8 }
import org.bitcoins.core.protocol.ln.LnHumanReadablePart
import org.bitcoins.core.protocol.ln.LnHumanReadablePart.parse
import org.slf4j.LoggerFactory
import scodec.bits.{ BitVector, ByteVector }

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

/**
 * A abstract class representing basic utility functions of Bech32
 * For more information on Bech32 please seee BIP173
 * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki]]
 */
sealed abstract class Bech32 {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  private val generators: Vector[Long] = Vector(
    UInt32("3b6a57b2").toLong,
    UInt32("26508e6d").toLong, UInt32("1ea119fa").toLong,
    UInt32("3d4233dd").toLong, UInt32("2a1462b3").toLong)

  private val u32Five = UInt32(5)
  private val u32Eight = UInt32(8)
  private val u8ThirtyTwo = UInt8(32)

  /**
   * Creates a checksum for the given byte vector according to BIP173
   * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32]]
   * @param u5s
   * @return
   */
  def createChecksum(u5s: Vector[UInt5]): Vector[UInt5] = {
    val z = UInt5.zero
    val polymod: Long = polyMod(u5s ++ Array(z, z, z, z, z, z)) ^ 1
    //[(polymod >> 5 * (5 - i)) & 31 for i in range(6)]

    val result: Vector[UInt5] = 0.until(6).map { i =>
      //((polymod >> five * (five - u)) & UInt8(31.toShort))
      UInt5(((polymod >> 5 * (5 - i)) & 31))
    }.toVector
    result
  }

  /**
   * Expands the human readable part of a bech32 address as per BIP173
   * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32]]
   * @param bytes
   * @return
   */
  def hrpExpand(bytes: ByteVector): Vector[UInt5] = {
    val x: ByteVector = bytes.map { b: Byte =>
      (b >> 5).toByte
    }
    val withZero: ByteVector = x ++ ByteVector.low(1)

    val y: ByteVector = bytes.map { char =>
      (char & 0x1f).toByte
    }
    val result = withZero ++ y

    UInt5.toUInt5s(result)
  }

  def polyMod(bytes: Vector[UInt5]): Long = {
    var chk: Long = 1
    bytes.map { v =>
      val b = chk >> 25
      //chk = (chk & 0x1ffffff) << 5 ^ v
      chk = (chk & 0x1ffffff) << 5 ^ v.toLong
      0.until(5).map { i: Int =>
        //chk ^= GEN[i] if ((b >> i) & 1) else 0
        if (((b >> i) & 1) == 1) {
          chk = chk ^ generators(i)
        }
      }
    }
    chk
  }

  /** Checks if the possible human readable part follows BIP173 rules */
  def checkHrpValidity[T](hrp: String, f: String => Try[T]): Try[T] = {
    @tailrec
    def loop(remaining: List[Char], accum: Seq[UInt8], isLower: Boolean, isUpper: Boolean): Try[Seq[UInt8]] = remaining match {
      case h :: t =>
        if (h < 33 || h > 126) {
          Failure(new IllegalArgumentException("Invalid character range for hrp, got: " + hrp))
        } else if (isLower && isUpper) {
          Failure(new IllegalArgumentException("HRP had mixed case, got: " + hrp))
        } else {
          loop(t, UInt8(h.toByte) +: accum, h.isLower || isLower, h.isUpper || isUpper)
        }
      case Nil =>
        if (isLower && isUpper) {
          Failure(new IllegalArgumentException("HRP had mixed case, got: " + hrp))
        } else {
          Success(accum.reverse)
        }
    }

    val isValid = loop(hrp.toCharArray.toList, Nil, false, false)

    isValid.flatMap { _ =>
      f(hrp.toLowerCase)
    }
  }

  /**
   * Takes in the data portion of a bech32 address and decodes it to a byte array
   * It also checks the validity of the data portion according to BIP173
   */
  def checkDataValidity(data: String): Try[Vector[UInt5]] = {
    @tailrec
    def loop(remaining: List[Char], accum: Vector[UInt5], hasUpper: Boolean, hasLower: Boolean): Try[Vector[UInt5]] = remaining match {
      case Nil => Success(accum.reverse)
      case h :: t =>
        if (!Bech32.charset.contains(h.toLower)) {
          Failure(new IllegalArgumentException("Invalid character in data of bech32 address, got: " + h))
        } else {
          if ((h.isUpper && hasLower) || (h.isLower && hasUpper)) {
            Failure(new IllegalArgumentException("Cannot have mixed case for bech32 address"))
          } else {
            val byte = Bech32.charset.indexOf(h.toLower).toByte
            require(byte >= 0 && byte < 32, "Not in valid range, got: " + byte)
            loop(
              remaining = t,
              accum = UInt5.fromByte(byte) +: accum,
              hasUpper = h.isUpper || hasUpper,
              hasLower = h.isLower || hasLower)
          }
        }
    }
    val payload: Try[Vector[UInt5]] = loop(data.toCharArray.toList, Vector.empty,
      false, false)

    payload
  }

  /** Encodes a bitvector to a bech32 string */
  def encodeBitVec(bitVec: BitVector): String = {
    @tailrec
    def loop(remaining: BitVector, accum: Vector[UInt5]): Vector[UInt5] = {
      if (remaining.length > 5) {

        val u5 = UInt5(remaining.take(5).toByte())

        val newRemaining = remaining.slice(5, remaining.size)

        loop(newRemaining, accum.:+(u5))

      } else {

        val u5 = UInt5(remaining.toByte())

        accum.:+(u5)
      }
    }

    val u5s = loop(bitVec, Vector.empty)

    encode5bitToString(u5s)
  }

  /**
   * Converts a byte vector to 5bit vector
   * and then serializes to bech32
   */
  def encode8bitToString(bytes: ByteVector): String = {
    val vec = UInt8.toUInt8s(bytes)
    encode8bitToString(vec)
  }

  /**
   * Converts a byte vector to 5bit vector
   * and then serializes to bech32
   */
  def encode8bitToString(bytes: Vector[UInt8]): String = {
    val b = from8bitTo5bit(bytes)
    encode5bitToString(b)
  }

  /** Takes a bech32 5bit array and encodes it to a string */
  def encode5bitToString(b: Vector[UInt5]): String = {
    b.map(b => Bech32.charset(b.toInt)).mkString
  }

  /** Converts a byte vector from 8bits to 5bits */
  def from8bitTo5bit(bytes: ByteVector): Vector[UInt5] = {
    val u8s = UInt8.toUInt8s(bytes)
    val u5s = NumberUtil.convertUInt8sToUInt5s(u8s)
    u5s
  }

  /** Converts a byte array from 8bits to base 5 bits */
  def from8bitTo5bit(u8s: Vector[UInt8]): Vector[UInt5] = {
    val bytes = UInt8.toBytes(u8s)
    from8bitTo5bit(bytes)
  }

  /** Decodes a byte array from 5bits to base 8bits */
  def from5bitTo8bit(b: Vector[UInt5]): Vector[UInt8] = {
    NumberUtil.convertUInt5sToUInt8(b)
  }

  private def handleEncodeTry[T](vecT: Try[Vector[T]]): Vector[T] = {
    //should always be able to encode a hex string to bech32
    vecT match {
      case Success(vec) => vec
      case Failure(err) =>
        logger.error(s"Failed to encode a vec to bech32. Vec: ${vecT} err: ${err.getMessage}")
        throw err
    }
  }

}

object Bech32 extends Bech32 {

  /** Separator used to separate the hrp & data parts of a bech32 addr */
  val separator = '1'

  /** https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki#bech32 */
  val charset: Vector[Char] = Vector(
    'q', 'p', 'z', 'r', 'y', '9', 'x', '8',
    'g', 'f', '2', 't', 'v', 'd', 'w', '0',
    's', '3', 'j', 'n', '5', '4', 'k', 'h',
    'c', 'e', '6', 'm', 'u', 'a', '7', 'l')
}
