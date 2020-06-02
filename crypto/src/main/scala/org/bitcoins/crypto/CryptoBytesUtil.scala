package org.bitcoins.crypto

import scodec.bits.{BitVector, ByteVector}

import scala.math.BigInt

/**
  * Created by chris on 2/26/16.
  */
trait CryptoBytesUtil {

  def decodeHex(hex: String): ByteVector = {
    if (hex.isEmpty) ByteVector.empty else ByteVector.fromValidHex(hex)
  }

  def encodeHex(bytes: ByteVector): String = bytes.toHex

  def encodeHex(byte: Byte): String = encodeHex(ByteVector(byte))

  /**
    * Encodes a long number to a hex string, pads it with an extra '0' char
    * if the hex string is an odd amount of characters.
    */
  def encodeHex(long: Long): String = {
    val hex = long.toHexString.length % 2 match {
      case 1      => "0" + long.toHexString
      case _: Int => long.toHexString
    }
    val needed = 16 - hex.length
    addPadding(needed, hex)
  }

  def encodeHex(int: Int): String = {
    val hex = int.toHexString.length % 2 match {
      case 1      => "0" + int.toHexString
      case _: Int => int.toHexString
    }
    val needed = 8 - hex.length
    addPadding(needed, hex)
  }

  def encodeHex(short: Short): String = {
    val bytes = ByteVector.fromShort(short)
    encodeHex(bytes)
  }

  def encodeHex(bigInt: BigInt): String = {
    ByteVector(bigInt.toByteArray).toHex
  }

  /** Tests if a given string is a hexadecimal string. */
  def isHex(str: String): Boolean = {
    //check valid characters & hex strings have to have an even number of chars
    str.matches("^[0-9a-f]+$") && (str.length % 2 == 0)
  }

  /** Converts a two character hex string to its byte representation. */
  def hexToByte(hex: String): Byte = {
    require(hex.length == 2)
    CryptoBytesUtil.decodeHex(hex).head
  }

  /** Flips the endianness of the give hex string. */
  def flipEndianness(hex: String): String = flipEndianness(decodeHex(hex))

  /** Flips the endianness of the given sequence of bytes. */
  def flipEndianness(bytes: ByteVector): String = encodeHex(bytes.reverse)

  private val Z: Char = '0'
  /**
    * Adds the amount padding bytes needed to fix the size of the hex string
    * for instance, ints are required to be 4 bytes. If the number is just 1
    * it will only take 1 byte. We need to pad the byte with an extra 3 bytes so the result is
    * 00000001 instead of just 1.
    */
  @inline final def addPadding(paddingNeeded: Int, hex: String): String = {
    val builder = new StringBuilder
    var counter = 0
    while (counter < paddingNeeded) {
      builder.append(Z)
      counter+=1
    }
    builder.appendAll(hex)
    builder.result()
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

  @inline
  final def toByteVector[T <: NetworkElement](h: Seq[T]): ByteVector = {
    ByteVector.concat(h.map(_.bytes))
  }
}

object CryptoBytesUtil extends CryptoBytesUtil
