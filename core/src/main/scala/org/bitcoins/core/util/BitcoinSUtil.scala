package org.bitcoins.core.util

import org.bitcoins.core.protocol.NetworkElement
import scodec.bits.{ BitVector, ByteVector }

import scala.math.BigInt

/**
 * Created by chris on 2/26/16.
 */
trait BitcoinSUtil {
  private val logger = BitcoinSLogger.logger
  def decodeHex(hex: String): ByteVector = {
    if (hex.isEmpty) ByteVector.empty else ByteVector.fromHex(hex).get
  }

  def encodeHex(bytes: ByteVector): String = bytes.toHex

  def encodeHex(byte: Byte): String = encodeHex(ByteVector(byte))

  /**
   * Encodes a long number to a hex string, pads it with an extra '0' char
   * if the hex string is an odd amount of characters.
   */
  def encodeHex(long: Long): String = {
    val hex = long.toHexString.length % 2 match {
      case 1 => "0" + long.toHexString
      case _: Int => long.toHexString
    }
    addPadding(16, hex)
  }

  def encodeHex(int: Int): String = {
    val hex = int.toHexString.length % 2 match {
      case 1 => "0" + int.toHexString
      case _: Int => int.toHexString
    }
    addPadding(8, hex)
  }

  def encodeHex(short: Short): String = {
    val bytes = ByteVector.fromShort(short)
    encodeHex(bytes)
  }

  def encodeHex(bigInt: BigInt): String = BitcoinSUtil.encodeHex(ByteVector(bigInt.toByteArray))

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
  def flipEndianness(bytes: ByteVector): String = encodeHex(bytes.reverse)

  def flipEndiannessBytes(bytes: ByteVector): ByteVector = bytes.reverse
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
  def bytesToBitVectors(bytes: ByteVector): BitVector = {
    bytes.toBitVector
  }

  /** Converts a byte to a bit vector representing that byte */
  def byteToBitVector(byte: Byte): BitVector = {
    BitVector.fromByte(byte)
  }

  /** Checks if the bit at the given index is set */
  def isBitSet(byte: Byte, index: Int): Boolean = ((byte >> index) & 1) == 1

  /** Converts a sequence of bit vectors to a sequence of bytes */
  def bitVectorToBytes(bits: BitVector): ByteVector = {
    bits.bytes
  }

  def toByteVector[T <: NetworkElement](h: Seq[T]): ByteVector = {
    h.foldLeft(ByteVector.empty)(_ ++ _.bytes)
  }

}

object BitcoinSUtil extends BitcoinSUtil
