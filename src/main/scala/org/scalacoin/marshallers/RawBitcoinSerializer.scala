package org.scalacoin.marshallers

import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/11/16.
 */
trait RawBitcoinSerializer[T] {

  /**
   * Reads a hexadecimal value and transforms it into the native
   * scala type T
   * @param hex
   * @return
   */
  def read(hex : String) : T = read(ScalacoinUtil.decodeHex(hex))

  def read(bytes : List[Byte]) : T

  /**
   * Takes a type T and writes it into the appropriate type T
   * @param t
   * @return
   */
  def write(t : T) : String


  /**
   * Adds the amount padding bytes needed to fix the size of the hex string
   * for instance, vouts are required to be 4 bytes. If the number is just 1
   * it will only take 1 byte. We need to pad the byte with an extra 3 bytes so the result is
   * 01000000 instead of just 01
   * @param bytesNeeded
   * @param hex
   * @return
   */
  def addPadding(charactersNeeded : Int, hex : String) : String = {
    val paddingNeeded = charactersNeeded - hex.size
    val padding = for { i <- 0 until paddingNeeded} yield "0"
    val paddedHex = hex + padding.mkString
    paddedHex
  }

  /**
   * Adds a preceding zero to a hex string.
   * Example: if '1' was passed in, it would return the hex string '01'
   * @param hex
   * @return
   */
  def addPrecedingZero(hex : String) = {
    if (hex.size == 1) "0" + hex else hex
  }




}
