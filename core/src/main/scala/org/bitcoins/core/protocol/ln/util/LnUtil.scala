package org.bitcoins.core.protocol.ln.util

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.util.Bech32

import scala.annotation.tailrec

/** Useful utility functions for the Lightning encoding / decoding */
abstract class LnUtil {

  /** The formula for this calculation is as follows:
    * Take the length of the Bech32 encoded input and divide it by 32.
    * Take the quotient, and encode this value as Bech32. Take the remainder and encode this value as Bech32.
    * Append these values to produce a valid Lighting Network `data_length` field.
    * Please see
    * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#examples Bolt11]]
    * for examples.
    */
  def createDataLength(bech32String: String): List[UInt5] = {
    val u5s = Bech32.decodeStringToU5s(bech32String)
    createDataLength(u5s.toList)
  }

  /** Creates the appropriate 10 bit vector for the data length of an element */
  def createDataLength(u5s: List[UInt5]): List[UInt5] = {
    val len = u5s.size
    val encodedNoPadding = encodeNumber(len)

    val encoded = {
      if (encodedNoPadding.size == 1) {
        UInt5.zero :: encodedNoPadding
      } else {
        encodedNoPadding
      }
    }

    val size = encoded.size
    require(size == 2, s"data_length must be 2 uint5s, got $size")

    encoded
  }

  def decodeDataLength(u5s: List[UInt5]): Long = {
    require(u5s.length == 2,
            s"Data Length is required to be 10 bits, got ${u5s.length}")
    decodeNumber(u5s).toLong
  }

  /** Returns a 5 bit bytevector with the encoded number for a LN invoice */
  @tailrec
  final def encodeNumber(
      len: BigInt,
      accum: List[UInt5] = List.empty): List[UInt5] = {
    val quotient = len / 32
    val remainder = UInt5(len % 32)
    if (quotient >= 32) {
      encodeNumber(quotient, remainder :: accum)
    } else if (quotient == 0) {
      remainder :: accum
    } else {
      val quo = UInt5.fromByte(quotient.toByte)
      quo :: remainder :: accum
    }

  }

  /** Decodes a number from Bech32 to a long */
  @tailrec
  final def decodeNumber(list: List[UInt5], accum: BigInt = 0): BigInt = {
    list match {
      case h :: Nil =>
        decodeNumber(Nil, h.toInt + accum)
      case h :: t =>
        val b32 = BigInt(32)
        val n = b32.pow(t.size)
        val newAccum = h.toBigInt * n + accum
        decodeNumber(t, newAccum)
      case Nil =>
        accum
    }
  }
}

object LnUtil extends LnUtil
