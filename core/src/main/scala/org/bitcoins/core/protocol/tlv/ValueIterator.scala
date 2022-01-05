package org.bitcoins.core.protocol.tlv

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{Int32, UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.tlv.TLV.{FALSE_BYTE, TRUE_BYTE}
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

case class ValueIterator(value: ByteVector) {

  private var index: Int = 0
  def finished: Boolean = current.isEmpty

  def current: ByteVector = {
    value.drop(index)
  }

  def skip(numBytes: Long): Unit = {
    index += numBytes.toInt
    ()
  }

  def skip(bytes: NetworkElement): Unit = {
    skip(bytes.byteSize)
    ()
  }

  def take(numBytes: Int): ByteVector = {
    require(current.length >= numBytes)
    val bytes = current.take(numBytes)
    skip(numBytes)
    bytes
  }

  /** IMPORTANT: This only works for factories which read off of
    * the front of a ByteVector without consuming the whole thing.
    * If this is not the case, you must specify how many bytes.
    */
  def take[E <: NetworkElement](factory: Factory[E]): E = {
    val elem = factory(current)
    skip(elem)
    elem
  }

  def take[E <: NetworkElement](factory: Factory[E], byteSize: Int): E = {
    val bytes = take(byteSize)
    factory(bytes)
  }

  def takeBits(numBits: Int): ByteVector = {
    require(numBits % 8 == 0,
            s"Must take a round byte number of bits, got $numBits")
    take(numBytes = numBits / 8)
  }

  def takeBigSize(): BigSizeUInt = {
    take(BigSizeUInt)
  }

  def takeBigSizePrefixed[E](takeFunc: Int => E): E = {
    val len = takeBigSize()
    takeFunc(len.toInt)
  }

  def takeBigSizePrefixedList[E](takeFunc: () => E): Vector[E] = {
    val len = takeBigSize()
    0.until(len.toInt).toVector.map { _ =>
      takeFunc()
    }
  }

  def takeU16(): UInt16 = {
    UInt16(takeBits(16))
  }

  def takeU16Prefixed[E](takeFunc: Int => E): E = {
    val len = takeU16()
    takeFunc(len.toInt)
  }

  def takeU16PrefixedList[E](takeFunc: () => E): Vector[E] = {
    val len = takeU16()
    0.until(len.toInt).toVector.map { _ =>
      takeFunc()
    }
  }

  def takeI32(): Int32 = {
    Int32(takeBits(32))
  }

  def takeU32(): UInt32 = {
    UInt32(takeBits(32))
  }

  def takeU64(): UInt64 = {
    UInt64(takeBits(64))
  }

  def takeSats(): Satoshis = {
    Satoshis(takeU64().toLong)
  }

  def takeBoolean(): Boolean = {
    take(1).head match {
      case FALSE_BYTE => false
      case TRUE_BYTE  => true
      case byte: Byte =>
        throw new RuntimeException(
          s"Boolean values must be 0x00 or 0x01, got $byte")
    }
  }

  def takeString(): NormalizedString = {
    val size = takeBigSize()
    val strBytes = take(size.toInt)
    NormalizedString(strBytes)
  }

  def takeSPK(): ScriptPubKey = {
    val len = takeU16().toInt
    ScriptPubKey.fromAsmBytes(take(len))
  }
}
