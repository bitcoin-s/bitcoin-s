package org.bitcoins.core.util

import org.slf4j.Logger

/**
 * Created by chris on 2/26/16.
 * Trait to implement ubiquitous factory functions across our codebase
 */
trait Factory[T] {

  /** Creates a T out of a hex string. */
  def fromHex(hex: String): T = fromBytes(BitcoinSUtil.decodeHex(hex))

  /** Creates a T out of a sequence of bytes. */
  def fromBytes(bytes: Seq[Byte]): T

  /** Creates a T out of a sequence of bytes. */
  def apply(bytes: Seq[Byte]): T = fromBytes(bytes)

  /** Creates a T from a hex string. */
  def apply(hex: String): T = fromHex(hex)

  def logger: Logger = BitcoinSLogger.logger
}
