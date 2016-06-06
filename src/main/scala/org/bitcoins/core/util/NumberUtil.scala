package org.bitcoins.core.util

import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.{CompactSizeUInt}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 2/8/16.
 */
trait NumberUtil extends BitcoinSLogger {

  /**
   * Takes a hex number and converts it into a signed number
   * used in the bitcoin numbering system
   * @param hex
   * @return
   */
  def toLong(hex : String) : Long = toLong(BitcoinSUtil.decodeHex(hex))

  /**
   * Takes a list of bytes and converts it in to signed number inside of bitcoins
   * numbering system
   * @param bytes
   * @return
   */
  def toLong(bytes : Seq[Byte]) : Long = {
    val reversedBytes = bytes.reverse
    if (bytes.size == 1 && bytes.head == -128) {
      //the case for negative zero
      0
    } else if (isPositive(bytes)) {
      if (firstByteAllZeros(reversedBytes.toList) && reversedBytes.size > 1) {
        parseLong(reversedBytes.slice(1,reversedBytes.size))
      } else parseLong(reversedBytes)
    } else {
      //remove the sign bit
      val removedSignBit = changeSignBitToPositive(reversedBytes.toList)
      if (firstByteAllZeros(removedSignBit)) -parseLong(removedSignBit.slice(1,removedSignBit.size))
      else -parseLong(removedSignBit)
    }
  }


  /**
   * Converts a long number to the representation of number inside of Bitcoin's number system
   * @param long
   * @return
   */
  def longToHex(long : Long) : String = {
    if (long > -1) {
      val bytes = toByteSeq(long)
      BitcoinSUtil.flipEndianess(BitcoinSUtil.encodeHex(bytes))
    } else {
      val bytes = toByteSeq(long.abs)
      //add sign bit
      val negativeNumberBytes = changeSignBitToNegative(bytes)
      val hex = BitcoinSUtil.encodeHex(negativeNumberBytes.reverse)
      hex
    }
  }

  /**
   * Determines if a given hex string is a positive number
   * @param hex
   * @return
   */
  def isPositive(hex : String) : Boolean = isPositive(BitcoinSUtil.decodeHex(hex))

  /**
   * Determines if a byte array is a positive or negative number
   * @param bytes
   * @return
   */
  def isPositive(bytes : Seq[Byte]) : Boolean = {
    if (bytes.isEmpty) false
    else {
      val result: Int = bytes(bytes.size-1) & 0x80
      if (result == 0x80) false else true
    }

  }

  def isNegative(hex : String) : Boolean = isNegative(BitcoinSUtil.decodeHex(hex))

  def isNegative(bytes : Seq[Byte]) : Boolean = {
    if (bytes.isEmpty) false else !isPositive(bytes)
  }

  /**
   * Change sign bit to positive
   * @param bytes
   * @return
   */
  def changeSignBitToPositive(bytes : Seq[Byte]) : Seq[Byte] = {
    val newByte : Byte = (bytes.head & 0x7F).toByte
    (newByte :: bytes.tail.toList)
  }

  def changeSignBitToPositive(hex : String) : Seq[Byte] = changeSignBitToPositive(BitcoinSUtil.decodeHex(hex))


  def changeSignBitToNegative(bytes : Seq[Byte]) : Seq[Byte] = {
    val newByte = (bytes.head | 0x80).toByte
    (newByte :: bytes.tail.toList)
  }

  def changeSignBitToNegative(hex : String) : Seq[Byte] = changeSignBitToNegative(BitcoinSUtil.decodeHex(hex))


  def firstByteAllZeros(hex : String) : Boolean = firstByteAllZeros(BitcoinSUtil.decodeHex(hex))

  def firstByteAllZeros(bytes : Seq[Byte]) : Boolean = {
    val lastByte = bytes.head
    (lastByte & 0xFF) == 0
  }


  def toByteSeq(long : Long) : Seq[Byte] = BigInt(long).toByteArray


  /**
   * Parses a VarInt from a string of hex characters
   * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
   * @param hex
   * @return
   */
  def parseCompactSizeUInt(hex : String) : CompactSizeUInt = parseCompactSizeUInt(BitcoinSUtil.decodeHex(hex))

  /**
   * Parses a CompactSizeUInt from a sequence of bytes
   * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
   * @param bytes
   * @return
   */
  def parseCompactSizeUInt(bytes : Seq[Byte]) : CompactSizeUInt = {
    require(bytes.size > 0, "Cannot parse a VarInt if the byte array is size 0")
    //8 bit number
    if (parseLong(bytes.head) < 253) CompactSizeUInt(parseLong(bytes.head),1)
    //16 bit number
    else if (parseLong(bytes.head) == 253) CompactSizeUInt(parseLong(bytes.slice(1,3).reverse),3)
    //32 bit number
    else if (parseLong(bytes.head) == 254) CompactSizeUInt(parseLong(bytes.slice(1,5).reverse),5)
    //64 bit number
    else CompactSizeUInt(parseLong(bytes.slice(1,9).reverse),9)
  }

  /**
   * Returns the size of a VarInt in the number of bytes
   * https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
   * @param byte
   * @return
   */
  def parseCompactSizeUIntSize(byte : Byte) : Long = {
    //8 bit number
    if (parseLong(byte) < 253) 1
    //16 bit number
    else if (parseLong(byte) == 253) 3
    //32 bit number
    else if (parseLong(byte) == 254) 5
    //64 bit number
    else 9
  }


  /**
   * Parses the compact size uint from a script signature
   * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
   * @param script
   * @return
   */
  def parseCompactSizeUInt(script : ScriptSignature) : CompactSizeUInt = {
    if (script.bytes.size <=252 ) {
      CompactSizeUInt(script.bytes.size,1)
    } else if (script.bytes.size <= 0xffff) {
      CompactSizeUInt(script.bytes.size,3)
    } else if (script.bytes.size <= 0xffffffff) {
      CompactSizeUInt(script.bytes.size,5)
    }
    else CompactSizeUInt(script.bytes.size,9)
  }

  /**
   * Parses a compact size uint from a script pubkey
   * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
   * @param scriptPubKey
   * @return
   */
  def parseCompactSizeUInt(scriptPubKey : ScriptPubKey) : CompactSizeUInt = {
    if (scriptPubKey.bytes.size <=252 ) {
      CompactSizeUInt(scriptPubKey.bytes.size,1)
    } else if (scriptPubKey.bytes.size <= 0xffff) {
      CompactSizeUInt(scriptPubKey.bytes.size,3)
    } else if (scriptPubKey.bytes.size <= 0xffffffff) {
      CompactSizeUInt(scriptPubKey.bytes.size,5)
    } else CompactSizeUInt(scriptPubKey.bytes.size,9)
  }

  private def parseLong(hex : String) : Long = java.lang.Long.parseLong(hex,16)

  private def parseLong(bytes : List[Byte]) : Long = parseLong(BitcoinSUtil.encodeHex(bytes))

  private def parseLong(byte : Byte) : Long = parseLong(List(byte))

  private def parseLong(bytes : Seq[Byte]) : Long = parseLong(bytes.toList)
}
