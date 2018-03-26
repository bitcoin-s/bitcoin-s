package org.bitcoins.core.util

import scala.math.BigInt

/**
 * Created by chris on 2/26/16.
 */
trait BitcoinSUtil {

  def decodeHex(hex: String): Seq[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte).toList
  }

  def encodeHex(bytes: Seq[Byte]): String = bytes.map("%02x".format(_)).mkString

  def encodeHex(byte: Byte): String = encodeHex(Seq(byte))

  /**
   * Encodes a long number to a hex string, pads it with an extra '0' char
   * if the hex string is an odd amount of characters.
   */
  def encodeHex(long: Long): String = {
    val hex = long.toHexString.length % 2 match {
      case 1      => "0" + long.toHexString
      case _: Int => long.toHexString
    }
    addPadding(16, hex)
  }

  def encodeHex(int: Int): String = {
    val hex = int.toHexString.length % 2 match {
      case 1      => "0" + int.toHexString
      case _: Int => int.toHexString
    }
    addPadding(8, hex)
  }

  def encodeHex(short: Short): String = {
    val hex = short.toHexString.length % 2 match {
      case 1      => "0" + short.toHexString
      case _: Int => short.toHexString
    }
    addPadding(4, hex)
  }

  def encodeHex(bigInt: BigInt): String = BitcoinSUtil.encodeHex(bigInt.toByteArray)

  /** Tests if a given string is a hexadecimal string. */
  def isHex(str: String): Boolean = {
    //check valid characters & hex strings have to have an even number of chars
    str.matches("^[0-9a-f]+$") && (str.length % 2 == 0)
  }

  /** Converts a two character hex string to its byte representation. */
  def hexToByte(hex: String): Byte = {
    require(hex.length == 2)
    BitcoinSUtil.decodeHex(hex).head
  }

  /** Flips the endianness of the give hex string. */
  def flipEndianness(hex: String): String = flipEndianness(decodeHex(hex))

  /** Flips the endianness of the given sequence of bytes. */
  def flipEndianness(bytes: Seq[Byte]): String = encodeHex(bytes.reverse)

  /**
   * Adds the amount padding bytes needed to fix the size of the hex string
   * for instance, ints are required to be 4 bytes. If the number is just 1
   * it will only take 1 byte. We need to pad the byte with an extra 3 bytes so the result is
   * 00000001 instead of just 1.
   */
  private def addPadding(charactersNeeded: Int, hex: String): String = {
    val paddingNeeded = charactersNeeded - hex.length
    val padding = for { i <- 0 until paddingNeeded } yield "0"
    val paddedHex = padding.mkString + hex
    paddedHex
  }

  /** Converts a sequence of bytes to a sequence of bit vectors */
  def bytesToBitVectors(bytes: Seq[Byte]): Seq[Seq[Boolean]] = bytes.map(byteToBitVector)

  /** Converts a byte to a bit vector representing that byte */
  def byteToBitVector(byte: Byte): Seq[Boolean] = {
    (0 to 7).map(index => isBitSet(byte, 7 - index))
  }

  /** Checks if the bit at the given index is set */
  def isBitSet(byte: Byte, index: Int): Boolean = ((byte >> index) & 1) == 1

  /** Converts a bit vector to a single byte -- the resulting byte is big endian */
  def bitVectorToByte(bits: Seq[Boolean]): Byte = {
    require(bits.size <= 8, "Cannot convert a bit vector to a byte when the size of the bit vector is larger than 8, got: " + bits)
    val b = bits.reverse
    val result: Seq[Int] = b.zipWithIndex.map {
      case (b, index) =>
        if (b) NumberUtil.pow2(index).toInt else 0
    }
    result.sum.toByte
  }

  /** Converts a sequence of bit vectors to a sequence of bytes */
  def bitVectorsToBytes(bits: Seq[Seq[Boolean]]): Seq[Byte] = bits.map(bitVectorToByte)

}

object BitcoinSUtil extends BitcoinSUtil
