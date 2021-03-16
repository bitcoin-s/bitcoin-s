package org.bitcoins.crypto

import scodec.bits.ByteVector

import java.math.BigInteger
import scala.math.BigInt

trait CryptoNumberUtil {

  /** Converts a sequence of bytes to a **big endian** unsigned integer */
  def toUnsignedInt(bytes: ByteVector): BigInt = {
    toUnsignedInt(bytes.toArray)
  }

  /** Converts a sequence of bytes to a **big endian** unsigned integer */
  def toUnsignedInt(bytes: Array[Byte]): BigInt = {
    BigInt(new BigInteger(1, bytes))
  }

  def uintToFieldElement(bytes: ByteVector): FieldElement = {
    FieldElement(toUnsignedInt(bytes))
  }

  /** Takes a hex string and parses it to a [[scala.math.BigInt BigInt]]. */
  def toBigInt(hex: String): BigInt = toBigInt(CryptoBytesUtil.decodeHex(hex))

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

}

object CryptoNumberUtil extends CryptoNumberUtil
