package org.bitcoins.core.protocol

import org.bitcoins.core.util.BitcoinSLogger
import scodec.bits.ByteVector

/**
  * Created by chris on 1/14/16.
  * This represents a element that can be serialized to
  * be sent over the network
  */
trait NetworkElement extends Any {

  /** The size of the NetworkElement in bytes. */
  def size: Long = bytes.size

  /** The hexadecimal representation of the NetworkElement */
  def hex: String = bytes.toHex

  /** The byte representation of the NetworkElement */
  def bytes: ByteVector

  def logger = BitcoinSLogger.logger
}
