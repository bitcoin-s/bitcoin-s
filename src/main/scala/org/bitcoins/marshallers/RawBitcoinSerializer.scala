package org.bitcoins.marshallers

import org.bitcoins.util.BitcoinSUtil

/**
 * Created by chris on 1/11/16.
 * A common trait for reading/writing bitcoin objects to/from bytes/hex
 */
trait RawBitcoinSerializer[T] extends RawBitcoinSerializerHelper {

  /**
   * Reads a hexadecimal value and transforms it into the native
   * scala type T
 *
   * @param hex
   * @return
   */
  def read(hex : String) : T = read(BitcoinSUtil.decodeHex(hex))

  /**
   * Reads in bytes and transforms it into the approriate scala type T
 *
   * @param bytes
   * @return
   */
  def read(bytes : List[Byte]) : T

  /**
   * Reads in bytes and transforms it into the approriate scala type T
 *
   * @param bytes
   * @return
   */
  def read(bytes : Seq[Byte]) : T = read(bytes.toList)

  /**
   * Takes a type T and writes it into the appropriate hexadecimal serialization for type T
 *
   * @param t
   * @return
   */
  def write(t : T) : String

}
