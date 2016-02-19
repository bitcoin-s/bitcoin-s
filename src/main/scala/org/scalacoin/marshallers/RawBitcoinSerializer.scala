package org.scalacoin.marshallers

import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/11/16.
 */
trait RawBitcoinSerializer[T] extends RawBitcoinSerializerHelper {

  /**
   * Reads a hexadecimal value and transforms it into the native
   * scala type T
   * @param hex
   * @return
   */
  def read(hex : String) : T = read(ScalacoinUtil.decodeHex(hex))

  /**
   * Reads in bytes and transforms it into the approriate scala type T
   * @param bytes
   * @return
   */
  def read(bytes : List[Byte]) : T

  /**
   * Reads in bytes and transforms it into the approriate scala type T
   * @param bytes
   * @return
   */
  def read(bytes : Seq[Byte]) : T = read(bytes.toList)

  /**
   * Takes a type T and writes it into the appropriate hexadecimal serialization for type T
   * @param t
   * @return
   */
  def write(t : T) : String

}
