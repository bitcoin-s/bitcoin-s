package org.bitcoins.crypto

import scodec.bits.ByteVector

/**
  * Created by chris on 2/26/16.
  * Trait to implement ubiquitous factory functions across our codebase
  */
abstract class Factory[+T] {

  /** Creates a T out of a hex string. */
  def fromHex(hex: String): T = fromBytes(BytesUtil.decodeHex(hex))

  /** Creates a T out of a hex string in little endian. */
  def fromHexLE(hex: String): T = fromBytesLE(BytesUtil.decodeHex(hex))

  /** Creates a T out of a sequence of bytes. */
  def fromBytes(bytes: ByteVector): T

  /** Creates a T out of a sequence of bytes in little endian. */
  def fromBytesLE(bytes: ByteVector): T = fromBytes(bytes.reverse)

  /** Creates a T out of a sequence of bytes. */
  def apply(bytes: ByteVector): T = fromBytes(bytes)

  /** Creates a T from a hex string. */
  def apply(hex: String): T = fromHex(hex)

  /** Allows a `def foo[C: Factory]()` construction. */
  implicit def self: Factory[T] = this
}
