package org.bitcoins.core.util

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}

import scala.math.BigInt

/**
 * Created by chris on 2/26/16.
 */
trait BitcoinSUtil {

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

  def encodeHex(int : Int) : String = encodeHex(int.toLong)

  def encodeHex(bigInt : BigInt) : String = BitcoinSUtil.encodeHex(bigInt.toByteArray)

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
    * Converts a two character hex string to its byte representation
    * @param hex
    * @return
    */
  def hexToByte(hex : String): Byte = {
    require(hex.size == 2)
    BitcoinSUtil.decodeHex(hex).head
  }

  /**
   * Flips the endianess of the give hex string
   * @param hex
   * @return
   */
  def flipEndianess(hex : String) : String = flipEndianess(decodeHex(hex))

  /**
   * Flips the endianess of the given sequence of bytes
   * @param bytes
   * @return
   */
  def flipEndianess(bytes : Seq[Byte]) : String = encodeHex(bytes.reverse)


}

object BitcoinSUtil extends BitcoinSUtil
