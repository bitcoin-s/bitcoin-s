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
      flipEndianess(BigInt(satoshis.value.toLong).toByteArray)
    }
  }

  def encodeHex(byte : Byte) : String = encodeHex(Seq(byte))

  /**
    * Encodes a long number to a hex string, pads it with an extra '0' char
    * if the hex string is an odd amount of characters
    * @param long
    * @return
    */
  def encodeHex(long : Long) : String = long.toHexString.length % 2 match {
    case 1 => "0" + long.toHexString
    case _ : Int => long.toHexString
  }
  /**
   * Tests if a given string is a hexadecimal string
   * @param str
   * @return
   */
  def isHex(str : String) = {
    //check valid characters & hex strings have to have an even number of chars
    str.matches("^[0-9a-f]+$") && (str.size % 2 == 0)
  }

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



}

object BitcoinSUtil extends BitcoinSUtil
