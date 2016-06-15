package org.bitcoins.core.util

import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.{CompactSizeUInt, CompactSizeUIntImpl}

/**
 * Created by chris on 2/8/16.
 */
trait NumberUtil extends BitcoinSLogger {

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
    if (parseLong(bytes.head) < 253) CompactSizeUIntImpl(parseLong(bytes.head),1)
    //16 bit number
    else if (parseLong(bytes.head) == 253) CompactSizeUIntImpl(parseLong(bytes.slice(1,3).reverse),3)
    //32 bit number
    else if (parseLong(bytes.head) == 254) CompactSizeUIntImpl(parseLong(bytes.slice(1,5).reverse),5)
    //64 bit number
    else CompactSizeUIntImpl(parseLong(bytes.slice(1,9).reverse),9)
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
      CompactSizeUIntImpl(script.bytes.size,1)
    } else if (script.bytes.size <= 0xffff) {
      CompactSizeUIntImpl(script.bytes.size,3)
    } else if (script.bytes.size <= 0xffffffff) {
      CompactSizeUIntImpl(script.bytes.size,5)
    }
    else CompactSizeUIntImpl(script.bytes.size,9)
  }

  /**
   * Parses a compact size uint from a script pubkey
   * https://bitcoin.org/en/developer-reference#compactsize-unsigned-integers
   * @param scriptPubKey
   * @return
   */
  def parseCompactSizeUInt(scriptPubKey : ScriptPubKey) : CompactSizeUInt = {
    if (scriptPubKey.bytes.size <=252 ) {
      CompactSizeUIntImpl(scriptPubKey.bytes.size,1)
    } else if (scriptPubKey.bytes.size <= 0xffff) {
      CompactSizeUIntImpl(scriptPubKey.bytes.size,3)
    } else if (scriptPubKey.bytes.size <= 0xffffffff) {
      CompactSizeUIntImpl(scriptPubKey.bytes.size,5)
    }
    else CompactSizeUIntImpl(scriptPubKey.bytes.size,9)
  }

  private def parseLong(hex : String) : Long = java.lang.Long.parseLong(hex,16)

  private def parseLong(bytes : List[Byte]) : Long = parseLong(BitcoinSUtil.encodeHex(bytes))

  private def parseLong(byte : Byte) : Long = parseLong(List(byte))

  private def parseLong(bytes : Seq[Byte]) : Long = parseLong(bytes.toList)
}

object NumberUtil extends NumberUtil
