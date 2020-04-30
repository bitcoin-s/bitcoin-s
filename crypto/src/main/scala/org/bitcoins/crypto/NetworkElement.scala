package org.bitcoins.crypto

import scodec.bits.ByteVector

/**
  * Created by chris on 1/14/16.
  * This represents a element that can be serialized to
  * be sent over the network
  */
trait NetworkElement extends Any {

  /** The size of the NetworkElement in bytes. */
  def byteSize: Long = bytes.size

  /** The hexadecimal representation of the NetworkElement */
  def hex: String = bytes.toHex

  /** The hexadecimal representation of the NetworkElement in little endian */
  def hexLE: String = bytesLE.toHex

  /** The byte representation of the NetworkElement */
  def bytes: ByteVector

  /** The byte representation of the NetworkElement in little endian */
  def bytesLE: ByteVector = bytes.reverse
}
