package org.bitcoins.util

import org.bitcoinj.core.{Base58, Utils}
import org.bitcoins.currency.{CurrencyUnits, CurrencyUnit}

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt

/**
 * Created by chris on 2/26/16.
 */
trait BitcoinSUtil extends NumberUtil {

  def hexToBigInt(hex : String) : BigInt = BigInt(hex, 16)

  def decodeHex(hex : String) : List[Byte] = Utils.HEX.decode(hex.trim).toList

  def encodeHex(bytes : Array[Byte]) : String = Utils.HEX.encode(bytes)

  def encodeHex(bytes : List[Byte]) : String = encodeHex(bytes.toSeq)

  def encodeHex(bytes : Seq[Byte]) : String = encodeHex(bytes.toArray)

  def encodeHex(unit : CurrencyUnit) : String = {
    val satoshis = CurrencyUnits.toSatoshis(unit)
    //TODO: this is ugly, clean this up. Shouldn't have to use .toLong
    flipHalfByte(encodeHex(BigInt(satoshis.value.toLong).toByteArray).reverse)
  }

  def encodeHex(byte : Byte) : String = Utils.HEX.encode(Array(byte))

  /**
   * Tests if a given string is a hexadecimal string
 *
   * @param str
   * @return
   */
  def isHex(str : String) = {
    try {
      decodeHex(str.trim)
      true
    } catch {
      case _ : Throwable => false
    }
  }

  def hexToLong(hex : String) : Long = toLong(hex)

  def hexToInt(hex : String) : Int = toLong(hex).toInt

  def decodeBase58(base58 : String) : Seq[Byte] = Base58.decode(base58).toList

  def encodeBase58(bytes : Seq[Byte]) : String = Base58.encode(bytes.toArray)

  /**
   * Flips the endianess of the give hex string
 *
   * @param hex
   * @return
   */
  def flipEndianess(hex : String) : String = encodeHex(decodeHex(hex).reverse)

  /**
   * Flips the endianess of the given sequence of bytes
 *
   * @param bytes
   * @return
   */
  def flipEndianess(bytes : Seq[Byte]) : String = flipEndianess(BitcoinSUtil.encodeHex(bytes))
  /**
   * Flips the hex chars in a hex strings
   * Example: abcd would become badc
   * https://stackoverflow.com/questions/34799611/easiest-way-to-flip-the-endianness-of-a-byte-in-scala/34802270#34802270
 *
   * @param hex
   * @return
   */
  def flipHalfByte(hex : String) = hex.grouped(2).map(_.reverse).mkString
}

object BitcoinSUtil extends BitcoinSUtil
