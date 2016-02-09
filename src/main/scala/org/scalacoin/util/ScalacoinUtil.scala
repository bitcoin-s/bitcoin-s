package org.scalacoin.util

import org.bitcoinj.core.{Base58, Utils}
import org.scalacoin.currency.{CurrencyUnits, CurrencyUnit}

import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt
/**
 * Created by chris on 7/26/15.
 */
trait ScalacoinUtil extends NumberUtil {

  def hexToBigInt(hex : String) : BigInt = BigInt(hex, 16)

  def decodeHex(hex : String) : List[Byte] = Utils.HEX.decode(hex.trim).toList

  def decodeHex(char : Char) : Byte = Utils.HEX.decode(char.toString).toList.head

  def encodeHex(bytes : Array[Byte]) : String = Utils.HEX.encode(bytes)

  def encodeHex(bytes : List[Byte]) : String = encodeHex(bytes.toArray)

  def encodeHex(unit : CurrencyUnit) : String = {
    val satoshis = CurrencyUnits.toSatoshis(unit)
    //TODO: this is ugly, clean this up. Shouldn't have to use .toLong
    flipHalfByte(encodeHex(BigInt(satoshis.value.toLong).toByteArray).reverse)
  }

  def encodeHex(byte : Byte) : String = Utils.HEX.encode(Array(byte))

  /**
   * Tests if a given string is a hexadecimal string
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

  /**
   * Converts an int to a list of bytes
   * @param x
   * @return
   */
  def intToByteList(x : Int) : List[Byte] = {
    //https://stackoverflow.com/questions/17870193/scala-serialize-int-to-arraybufferbyte-bit-twiddle-goes-wrong
    val intBytes:Int = 4 // int is 4 bytes
    val sizeByte:Short = 8
    val buf = new ArrayBuffer[Byte](intBytes)
    for(i <- 0 until intBytes) {
      buf += ((x >>> (intBytes - i - 1 << 3)) & 0xFF).toByte
    }
    buf.toList
  }

  def hexToLong(hex : String) : Long = toLong(hex)

  def decodeBase58(base58 : String) : List[Byte] = Base58.decode(base58).toList

  def encodeBase58(bytes : List[Byte]) : String = Base58.encode(bytes.toArray)

  /**
   * Converts a little endian encoded hex string to a big endian encoded hex string
   * @param hex
   * @return
   */
  def littleEndianToBigEndian(hex : String) = encodeHex(decodeHex(hex).reverse)
  /**
   * Flips the hex chars in a hex strings
   * Example: abcd would become badc
   * https://stackoverflow.com/questions/34799611/easiest-way-to-flip-the-endianness-of-a-byte-in-scala/34802270#34802270
   * @param hex
   * @return
   */
  def flipHalfByte(hex : String) = hex.grouped(2).map(_.reverse).mkString
}

object ScalacoinUtil extends ScalacoinUtil