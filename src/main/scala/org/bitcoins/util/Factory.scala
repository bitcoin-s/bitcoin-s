package org.bitcoins.util

/**
 * Created by chris on 2/26/16.
 * Trait to implement ubiquitous factory functions across our codebase
 */
trait Factory[T] {

  /**
   * Creates a T out of a hex string
   * @param hex
   * @return
   */
  def fromHex(hex : String) : T = fromBytes(BitcoinSUtil.decodeHex(hex))

  /**
   * Creates a T out of a sequence of bytes
   * @param bytes
   * @return
   */
  def fromBytes(bytes : Seq[Byte]) : T

}
