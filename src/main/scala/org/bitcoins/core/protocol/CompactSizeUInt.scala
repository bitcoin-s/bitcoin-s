package org.bitcoins.core.protocol

import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.script.constant.ScriptNumberUtil
import org.bitcoins.core.util.BitcoinSUtil

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
    case 3 => "fd" + ScriptNumberUtil.longToHex(num)
    case 5 => "fe" + ScriptNumberUtil.longToHex(num)
    case _ => "ff" + ScriptNumberUtil.longToHex(num)
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





