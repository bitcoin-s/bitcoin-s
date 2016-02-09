package org.scalacoin.util

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
    if (isPositive(bytes)) {
      logger.debug("bytes are positive")
      if (firstByteAllZeros(reversedBytes)) {
        parseLong(reversedBytes.slice(1,reversedBytes.size))
      } else parseLong(reversedBytes)
    }
    else {
      //remove the sign bit
      val removedSignBit : List[Byte] = changeSignBitToPositive(reversedBytes)
      if (firstByteAllZeros(removedSignBit)) -parseLong(removedSignBit.slice(1,removedSignBit.size))
      else -parseLong(removedSignBit)
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


  def firstByteAllZeros(hex : String) : Boolean = firstByteAllZeros(ScalacoinUtil.decodeHex(hex))

  def firstByteAllZeros(bytes : List[Byte]) : Boolean = {
    val lastByte = bytes.head
    (lastByte & 0xFF) == 0
  }

  private def parseLong(hex : String) : Long = java.lang.Long.parseLong(hex,16)

  private def parseLong(bytes : List[Byte]) : Long = parseLong(ScalacoinUtil.encodeHex(bytes))
}
