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

}
