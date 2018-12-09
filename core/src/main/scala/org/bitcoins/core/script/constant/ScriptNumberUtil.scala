package org.bitcoins.core.script.constant

import org.bitcoins.core.util.BitcoinSUtil
import scodec.bits.ByteVector

/**
  * Created by chris on 6/5/16.
  * Numbers in script are unique in the fact that they don't follow a conventional signed numbering system
  * such as ones complement or twos complement. The bitcoin protocol uses little endian notation which means the most
  * significant bit indicates the sign on the number we are interpreting. The rest of the bits are used to determine
  * what that number is. See this irc log for more info
  * https://botbot.me/freenode/bitcoin-core-dev/2016-06-06/?tz=America/Chicago
  */
trait ScriptNumberUtil {

  /**
    * Takes a hex number and converts it into a signed number
    * used in the bitcoin script's numbering system.
    * This function interprets the bytes as little endian numbers
    * This should only be used for numbers inside of Script
    *
    * @param hex
    * @return
    */
  def toLong(hex: String): Long = toLong(BitcoinSUtil.decodeHex(hex))

  /**
    * Takes in a hex string and converts it into a signed number
    * This function interprets the bytes as little endian numbers
    * This should only be used for numbers inside of Script
    *
    * @param hex
    * @return
    */
  def toInt(hex: String): Int = toInt(BitcoinSUtil.decodeHex(hex))

  /**
    * Takes in a sequence of bytes and converts it into a signed number
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

  /**
    * Takes a sequence of bytes and converts it in to signed number inside of bitcoin
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

  /**
    * Determines if a byte array is a positive or negative number
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

  /**
    * Change sign bit to positive
    *
    * @param bytes
    * @return
    */
  def changeSignBitToPositive(bytes: ByteVector): ByteVector = {
    val newByte: Byte = (bytes.head & 0x7F).toByte
    (newByte +: bytes.tail)
  }

  def firstByteAllZeros(bytes: ByteVector): Boolean = {
    val lastByte = bytes.head
    (lastByte & 0xFF) == 0
  }

  private def parseLong(bytes: ByteVector): Long =
    parseLong(BitcoinSUtil.encodeHex(bytes))

  private def parseLong(hex: String): Long = java.lang.Long.parseLong(hex, 16)

  /**
    * Converts a long number to the representation of number inside of Bitcoin script's number system
    *
    * @param long
    * @return
    */
  def longToHex(long: Long): String = {
    if (long > -1) {
      val bytes = toByteVec(long)
      BitcoinSUtil.flipEndianness(BitcoinSUtil.encodeHex(bytes))
    } else {
      val bytes = toByteVec(long.abs)
      //add sign bit
      val negativeNumberBytes = changeSignBitToNegative(bytes)
      val hex = BitcoinSUtil.encodeHex(negativeNumberBytes.reverse)
      hex
    }
  }

  def toByteVec(long: Long): ByteVector = {
    ByteVector(BigInt(long).toByteArray)
  }

  /**
    * Determines if a given hex string is a positive number
    *
    * @param hex
    * @return
    */
  def isPositive(hex: String): Boolean = isPositive(BitcoinSUtil.decodeHex(hex))

  def isNegative(hex: String): Boolean = isNegative(BitcoinSUtil.decodeHex(hex))

  def isNegative(bytes: ByteVector): Boolean = {
    if (bytes.isEmpty) false else !isPositive(bytes)
  }

  def changeSignBitToPositive(hex: String): ByteVector =
    changeSignBitToPositive(BitcoinSUtil.decodeHex(hex))

  def changeSignBitToNegative(hex: String): ByteVector =
    changeSignBitToNegative(BitcoinSUtil.decodeHex(hex))

  def changeSignBitToNegative(bytes: ByteVector): ByteVector = {
    val newByte = (bytes.head | 0x80).toByte
    (newByte +: bytes.tail)
  }

  def firstByteAllZeros(hex: String): Boolean =
    firstByteAllZeros(BitcoinSUtil.decodeHex(hex))

}

object ScriptNumberUtil extends ScriptNumberUtil
