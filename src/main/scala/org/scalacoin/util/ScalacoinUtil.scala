package org.scalacoin.util

import org.bitcoinj.core.{Base58, Utils}
import org.scalacoin.currency.{CurrencyUnits, CurrencyUnit}

import scala.math.BigInt
/**
 * Created by chris on 7/26/15.
 */
trait ScalacoinUtil {

  def hexToBigInt(hex : String) : BigInt = BigInt(hex, 16)

  def decodeHex(hex : String) : List[Byte] = Utils.HEX.decode(hex).toList

  def encodeHex(bytes : Array[Byte]) : String = Utils.HEX.encode(bytes)

  def encodeHex(bytes : List[Byte]) : String = encodeHex(bytes.toArray)

  def encodeHex(unit : CurrencyUnit) : String = {
    val satoshis = CurrencyUnits.toSatoshis(unit)
    //TODO: this is ugly, clean this up. Shouldn't have to use .toLong
    flipHalfByte(encodeHex(BigInt(satoshis.value.toLong).toByteArray).reverse)
  }

  def encodeHex(byte : Byte) : String = Utils.HEX.encode(Array(byte))

  def decodeBase58(base58 : String) : List[Byte] = Base58.decode(base58).toList

  def encodeBase58(bytes : List[Byte]) : String = Base58.encode(bytes.toArray)

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