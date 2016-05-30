package org.bitcoins.core.util

import org.bitcoins.core.currency.{CurrencyUnits, CurrencyUnit}
import scala.math.BigInt

/**
 * Created by chris on 2/26/16.
 */
trait BitcoinSUtil extends NumberUtil {

  def hexToBigInt(hex : String) : BigInt = BigInt(hex, 16)

  def decodeHex(hex : String) : Seq[Byte] = {
    hex.replaceAll("[^0-9A-Fa-f]", "").sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte).toList
  }

  def encodeHex(bytes : Seq[Byte]) : String = bytes.map("%02x".format(_)).mkString

  def encodeHex(unit : CurrencyUnit) : String = {
    val satoshis = CurrencyUnits.toSatoshis(unit)
    if (satoshis == CurrencyUnits.negativeSatoshi) {
      "ffffffffffffffff"
    } else {
      //TODO: this is ugly, clean this up. Shouldn't have to use .toLong
      flipHalfByte(encodeHex(BigInt(satoshis.value.toLong).toByteArray).reverse)
    }
  }

  def encodeHex(byte : Byte) : String = encodeHex(Seq(byte))

  /**
   * Tests if a given string is a hexadecimal string
   * @param str
   * @return
   */
  def isHex(str : String) = {
    //check valid characters & hex strings have to have an even number of chars
    str.matches("^[0-9a-f]+$") && (str.size % 2 == 0)
  }

  def hexToLong(hex : String) : Long = toLong(hex)

  def hexToInt(hex : String) : Int = toLong(hex).toInt

  /**
   * Flips the endianess of the give hex string
   * @param hex
   * @return
   */
  def flipEndianess(hex : String) : String = encodeHex(decodeHex(hex).reverse)

  /**
   * Flips the endianess of the given sequence of bytes
   * @param bytes
   * @return
   */
  def flipEndianess(bytes : Seq[Byte]) : String = flipEndianess(BitcoinSUtil.encodeHex(bytes))
  /**
   * Flips the hex chars in a hex strings
   * Example: abcd would become badc
   * https://stackoverflow.com/questions/34799611/easiest-way-to-flip-the-endianness-of-a-byte-in-scala/34802270#34802270
   * @param hex
   * @return
   */
  def flipHalfByte(hex : String) = hex.grouped(2).map(_.reverse).mkString


}

object BitcoinSUtil extends BitcoinSUtil
