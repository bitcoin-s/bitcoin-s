package org.bitcoins.core.protocol

import org.bitcoins.core.util.{BitcoinSUtil}

/**
 * Created by chris on 7/14/15.
 */


trait CompactSizeUInt {

  /**
   * The number parsed from VarInt
   * @return
   */
  def num : Long
  /**
   * The length of the VarInt in bytes
   * @return
   */
  def size : Long

  def hex = size match {
    case 1 => if (num.toHexString.size == 1) "0" + num.toHexString else num.toHexString
    case 3 =>
      println("Num: " +num)
      "fd" + BitcoinSUtil.encodeHex(BitcoinSUtil.toByteSeq(num).reverse)
    case 5 => "fe" + BitcoinSUtil.encodeHex(BitcoinSUtil.toByteSeq(num).reverse)
    case _ => "ff" + BitcoinSUtil.encodeHex(BitcoinSUtil.toByteSeq(num).reverse)
  }



}

object CompactSizeUInt {
  private sealed case class CompactSizeUIntImpl(num : Long, size : Long) extends CompactSizeUInt
  def apply(num : Long, size : Long) : CompactSizeUInt = {
    CompactSizeUIntImpl(num,size)
  }

  /**
    * This function is responsible for calculating what the compact size unsigned integer is for a
    * sequence of bytes
    * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
    * @param bytes
    * @return
    */
  def calculateCompactSizeUInt(bytes : Seq[Byte]) : CompactSizeUInt = {
    //means we can represent the number with a single byte
    if (bytes.size <= 0xff) CompactSizeUInt(bytes.size,1)
    // can be represented with two bytes
    else if (bytes.size <= 0xffff) CompactSizeUInt(bytes.size,3)
    //can be represented with 4 bytes
    else if (bytes.size <= 0xffffffff) CompactSizeUInt(bytes.size,5)
    else CompactSizeUInt(bytes.size,9)
  }

  /**
    * Responsible for calculating what the compact size uint is for this hex string
    * @param hex
    * @return
    */
  def calculateCompactSizeUInt(hex : String) : CompactSizeUInt = calculateCompactSizeUInt(BitcoinSUtil.decodeHex(hex))
}





