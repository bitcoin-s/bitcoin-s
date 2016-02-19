package org.scalacoin.util

import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.protocol.{VarIntImpl, VarInt}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 2/8/16.
 */
trait NumberUtil {


  private def logger = LoggerFactory.getLogger(this.getClass())

  def toLong(hex : String) : Long = toLong(ScalacoinUtil.decodeHex(hex))

  def toLong(bytes : List[Byte]) : Long = {
    logger.debug("bytes: " + bytes)
    val reversedBytes = bytes.reverse
    if (bytes.size == 1 && bytes.head == -128) {
      //the case for negative zero
      0
    } else if (isPositive(bytes)) {
      if (firstByteAllZeros(reversedBytes) && reversedBytes.size > 1) {
        parseLong(reversedBytes.slice(1,reversedBytes.size))
      } else parseLong(reversedBytes)
    } else {
      //remove the sign bit
      val removedSignBit : List[Byte] = changeSignBitToPositive(reversedBytes)
      if (firstByteAllZeros(removedSignBit)) -parseLong(removedSignBit.slice(1,removedSignBit.size))
      else -parseLong(removedSignBit)
    }
  }


  def longToHex(long : Long) : String = {
    if (long > -1) {
      val bytes = toByteList(long)
      ScalacoinUtil.encodeHex(bytes)
    } else {
      val bytes = toByteList(long.abs)
      //add sign bit
      val negativeNumberBytes : List[Byte] = changeSignBitToNegative(bytes)
      val hex = ScalacoinUtil.encodeHex(negativeNumberBytes.reverse)
      hex
    }
  }

  /**
   * Determines if a given hex string is a positive number
   * @param hex
   * @return
   */
  def isPositive(hex : String) : Boolean = isPositive(ScalacoinUtil.decodeHex(hex))

  /**
   * Determines if a byte array is a positive or negative number
   * @param bytes
   * @return
   */
  def isPositive(bytes : List[Byte]) = {
    val result: Int = bytes(bytes.size-1) & 0x80
    if (result == 0x80) false else true
  }

  def isNegative(hex : String) = !isPositive(hex)

  def isNegative(bytes : List[Byte]) = !isPositive(bytes)

  /**
   * Change sign bit to positive
   * @param bytes
   * @return
   */
  def changeSignBitToPositive(bytes : List[Byte]) : List[Byte] = {
    val newByte : Byte = (bytes.head & 0x7F).toByte
    newByte :: bytes.tail
  }

  def changeSignBitToPositive(hex : String) : List[Byte] = changeSignBitToPositive(ScalacoinUtil.decodeHex(hex))

  def changeSignBitToNegative(bytes : List[Byte]) : List[Byte] = {
    val newByte = (bytes.head | 0x80).toByte
    (newByte :: bytes.tail)
  }

  def changeSignBitToNegative(hex : String) : List[Byte] = changeSignBitToNegative(ScalacoinUtil.decodeHex(hex))


  def firstByteAllZeros(hex : String) : Boolean = firstByteAllZeros(ScalacoinUtil.decodeHex(hex))

  def firstByteAllZeros(bytes : List[Byte]) : Boolean = {
    val lastByte = bytes.head
    (lastByte & 0xFF) == 0
  }


  def toByteList(long : Long) = BigInt(long).toByteArray.toList

  /**
   * Parses a VarInt from a string of hex characters
   * https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
   * @param hex
   * @return
   */
  def parseVarInt(hex : String) : VarInt = parseVarInt(ScalacoinUtil.decodeHex(hex))

  /**
   * Parses a VarInt from a sequence of bytes
   * https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
   * @param bytes
   * @return
   */
  def parseVarInt(bytes : Seq[Byte]) : VarInt = {
    require(bytes.size > 0, "Cannot parse a VarInt if the byte array is size 0")
    //8 bit number
    if (parseLong(bytes.head) < 253) VarIntImpl(parseLong(bytes.head),1)
    //16 bit number
    else if (parseLong(bytes.head) == 253) VarIntImpl(parseLong(bytes.slice(1,3).reverse),3)
    //32 bit number
    else if (parseLong(bytes.head) == 254) VarIntImpl(parseLong(bytes.slice(1,5).reverse),5)
    //64 bit number
    else VarIntImpl(parseLong(bytes.slice(1,9).reverse),9)
  }

  /**
   * Returns the size of a VarInt in the number of bytes
   * https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
   * @param byte
   * @return
   */
  def parseVarIntSize(byte : Byte) : Long = {
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
   * Parses a VarInt from a sequence of bytes
   * https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
   * @param bytes
   * @return
   */
  def parseVarInt(scriptSig : ScriptSignature) : VarInt = {
    //largest 8 uint
    val size = scriptSig.size
    if (size < 255) VarIntImpl(size,1)
    else if (size < 65536) VarIntImpl(size,3)
    else if (size < 4294967296L) VarIntImpl(size,5)
    else VarIntImpl(size,9)
  }


  private def parseLong(hex : String) : Long = java.lang.Long.parseLong(hex,16)

  private def parseLong(bytes : List[Byte]) : Long = parseLong(ScalacoinUtil.encodeHex(bytes))

  private def parseLong(byte : Byte) : Long = parseLong(List(byte))

  private def parseLong(bytes : Seq[Byte]) : Long = parseLong(bytes.toList)
}
