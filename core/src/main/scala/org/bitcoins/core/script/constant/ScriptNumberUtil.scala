package org.bitcoins.core.script.constant

import org.bitcoins.core.util.BytesUtil
import scodec.bits.ByteVector

/** Created by chris on 6/5/16.
  * Numbers in script are unique in the fact that they don't follow a conventional signed numbering system
  * such as ones complement or twos complement. The bitcoin protocol uses little endian notation which means the most
  * significant bit indicates the sign on the number we are interpreting. The rest of the bits are used to determine
  * what that number is. See this irc log for more info
  * https://botbot.me/freenode/bitcoin-core-dev/2016-06-06/?tz=America/Chicago
  */
trait ScriptNumberUtil {

  /** Takes a hex number and converts it into a signed number
    * used in the bitcoin script's numbering system.
    * This function interprets the bytes as little endian numbers
    * This should only be used for numbers inside of Script
    *
    * @param hex
    * @return
    */
  def toLong(hex: String): Long = toLong(BytesUtil.decodeHex(hex))

  /** Takes in a hex string and converts it into a signed number
    * This function interprets the bytes as little endian numbers
    * This should only be used for numbers inside of Script
    *
    * @param hex
    * @return
    */
  def toInt(hex: String): Int = toInt(BytesUtil.decodeHex(hex))

  /** Takes in a sequence of bytes and converts it into a signed number
    * This should only be used for numbers inside of Script
    *
    * @param bytes
    * @return
    */
  def toInt(bytes: ByteVector): Int = {
    require(bytes.size <= 4,
            "We cannot have an integer with more than 4 bytes (32 bits)")
    toLong(bytes).toInt
  }

  /** Takes a sequence of bytes and converts it in to signed number inside of bitcoin
    * script's numbering system
    * This function interprets the bytes as little endian numbers
    * This should only be used for numbers inside of Script
    *
    * @param bytes
    * @return
    */
  def toLong(bytes: ByteVector): Long = {
    val reversedBytes = bytes.reverse
    if (bytes.size == 1 && bytes.head == -128) {
      //the case for negative zero
      0
    } else if (isPositive(bytes)) {
      if (firstByteAllZeros(reversedBytes) && reversedBytes.size > 1) {
        parseLong(reversedBytes.slice(1, reversedBytes.size))
      } else parseLong(reversedBytes)
    } else {
      //remove the sign bit
      val removedSignBit = changeSignBitToPositive(reversedBytes)
      if (firstByteAllZeros(removedSignBit))
        -parseLong(removedSignBit.slice(1, removedSignBit.size))
      else -parseLong(removedSignBit)
    }
  }

  /** Determines if a byte array is a positive or negative number
    *
    * @param bytes
    * @return
    */
  def isPositive(bytes: ByteVector): Boolean = {
    if (bytes.isEmpty) false
    else {
      val result: Int = bytes(bytes.size - 1) & 0x80
      if (result == 0x80) false else true
    }

  }

  /** Change sign bit to positive
    *
    * @param bytes
    * @return
    */
  def changeSignBitToPositive(bytes: ByteVector): ByteVector = {
    val newByte: Byte = (bytes.head & 0x7f).toByte
    (newByte +: bytes.tail)
  }

  def firstByteAllZeros(bytes: ByteVector): Boolean = {
    val lastByte = bytes.head
    (lastByte & 0xff) == 0
  }

  private def parseLong(bytes: ByteVector): Long =
    parseLong(BytesUtil.encodeHex(bytes))

  private def parseLong(hex: String): Long = java.lang.Long.parseLong(hex, 16)

  /** Converts a long number to the representation of number inside of Bitcoin script's number system
    *
    * @param long
    * @return
    */
  def longToHex(long: Long): String = {
    longToByteVector(long).toHex
  }

  /** Converts a long number to the bytevec representation in Script */
  def longToByteVector(long: Long): ByteVector = {
    if (long == 0) ByteVector.empty
    else if (long > -1) {
      val bytes = toByteVec(long)
      bytes.reverse
    } else {
      val bytes = toByteVec(long.abs)
      //add sign bit
      val negativeNumberBytes = changeSignBitToNegative(bytes)
      negativeNumberBytes.reverse
    }
  }

  def toByteVec(long: Long): ByteVector = {
    ByteVector(BigInt(long).toByteArray)
  }

  /** Determines if a given hex string is a positive number
    *
    * @param hex
    * @return
    */
  def isPositive(hex: String): Boolean = isPositive(BytesUtil.decodeHex(hex))

  def isNegative(hex: String): Boolean = isNegative(BytesUtil.decodeHex(hex))

  def isNegative(bytes: ByteVector): Boolean = {
    if (bytes.isEmpty) false else !isPositive(bytes)
  }

  def changeSignBitToPositive(hex: String): ByteVector =
    changeSignBitToPositive(BytesUtil.decodeHex(hex))

  def changeSignBitToNegative(hex: String): ByteVector =
    changeSignBitToNegative(BytesUtil.decodeHex(hex))

  def changeSignBitToNegative(bytes: ByteVector): ByteVector = {
    val newByte = (bytes.head | 0x80).toByte
    (newByte +: bytes.tail)
  }

  def firstByteAllZeros(hex: String): Boolean =
    firstByteAllZeros(BytesUtil.decodeHex(hex))

  /** Checks if the two given [[ScriptNumber numbers]] are equivalent to zero
    * in Script. Unfortunatey Script is one's complement which means we have
    * things like negative zero, and also there isn't an enforcement of a
    * minimal representation of zero, which means 0x00 = 0x0000 = 0x0000000.. == OP_0
    */
  def isZero(x: ScriptNumber): Boolean = {
    val xIsFalse = x == ScriptNumber.zero || x == OP_0
    val isNegZero = x == ScriptNumber.negativeZero
    val isZero = x.toLong == 0
    xIsFalse || isNegZero || isZero
  }
}

object ScriptNumberUtil extends ScriptNumberUtil
