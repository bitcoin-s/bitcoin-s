package org.bitcoins.crypto

import scodec.bits.ByteVector

import scala.util.Try

/** Created by chris on 2/26/16.
  * Trait to implement ubiquitous factory functions across our codebase
  */
abstract class Factory[+T] {

  /** Creates a T out of a hex string. */
  def fromHex(hex: String): T = fromBytes(CryptoBytesUtil.decodeHex(hex))

  /** Deserializes the given hex string to a T
    * If the hex is not correct, [[None]] is returned
    */
  def fromHexOpt(hex: String): Option[T] = {
    fromHexT(hex).toOption
  }

  /** Deserializes the given hex string
    * if the hex is not correct, we give you a [[Failure]]
    */
  def fromHexT(hex: String): Try[T] = {
    Try(fromHex(hex))
  }

  /** Creates a T out of a hex string in little endian. */
  def fromHexLE(hex: String): T = fromBytesLE(CryptoBytesUtil.decodeHex(hex))

  /** Creates a T out of a sequence of bytes. */
  def fromBytes(bytes: ByteVector): T

  /** Deserializes the given [[ByteVector]] to a T
    * If the [[ByteVector]] is not correct, [[None]] is returned
    */
  def fromBytesOpt(bytes: ByteVector): Option[T] = {
    fromBytesT(bytes).toOption
  }

  /** Deserializes the given [[ByteVector]] string
    * if the [[ByteVector]] is not correct, we give you a [[Failure]]
    */
  def fromBytesT(bytes: ByteVector): Try[T] = {
    Try(fromBytes(bytes))
  }

  /** Creates a T out of a sequence of bytes in little endian. */
  def fromBytesLE(bytes: ByteVector): T = fromBytes(bytes.reverse)

  /** Creates a T out of a sequence of bytes. */
  def apply(bytes: ByteVector): T = fromBytes(bytes)

  /** Creates a T from a hex string. */
  def apply(hex: String): T = fromHex(hex)

  /** Allows a `def foo[C: Factory]()` construction. */
  implicit def self: Factory[T] = this
}
