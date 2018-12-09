package org.bitcoins.core.protocol.ln.util

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.util.Bech32

import scala.annotation.tailrec

trait LnUtil {

  /**
    * The formula for this calculation is as follows:
    * Take the length of the Bech32 encoded input and divide it by 32.
    * Take the quotient, and encode this value as Bech32. Take the remainder and encode this value as Bech32.
    * Append these values to produce a valid Lighting Network `data_length` field.
    * Please see
    * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#examples Bolt11]]
    * for examples.
    */
  def createDataLength(bech32String: String): Vector[UInt5] = {
    val u5s = Bech32.decodeStringToU5s(bech32String)
    createDataLength(u5s)
  }

  /** Creates the appropriate 10 bit vector for the data length of an element */
  def createDataLength(u5s: Vector[UInt5]): Vector[UInt5] = {
    val len = u5s.size
    val encodedNoPadding = encodeNumber(len)

    val encoded = {
      if (encodedNoPadding.size == 1) {
        UInt5.zero +: encodedNoPadding
      } else {
        encodedNoPadding
      }
    }

    val size = encoded.size
    require(size == 2, s"data_length must be 2 uint5s, got $size")

    encoded
  }

  def decodeDataLength(u5s: Vector[UInt5]): Long = {
    require(u5s.length == 2,
            s"Data Length is required to be 10 bits, got ${u5s.length}")
    decodeNumber(u5s)
  }

  /** Returns a 5 bit bytevector with the encoded number for a LN invoice */
  @tailrec
  final def encodeNumber(
      len: BigInt,
      accum: Vector[UInt5] = Vector.empty): Vector[UInt5] = {
    val quotient = len / 32
    val remainder = UInt5(len % 32)
    if (quotient >= 32) {
      encodeNumber(quotient, remainder +: accum)
    } else if (quotient == 0) {
      remainder +: accum
    } else {
      val quo = UInt5.fromByte(quotient.toByte)
      val v = Vector(quo, remainder)
      v ++ accum
    }

  }

  @tailrec
  final def decodeNumber(vector: Vector[UInt5], accum: Long = 0): Long = {

    if (vector.isEmpty) accum
    else if (vector.size == 1) {
      decodeNumber(vector.tail, vector.head.toInt + accum)
    } else {
      val n = BigInt(32).pow(vector.size - 1)
      val newAccum = vector.head.toBigInt * n + accum
      decodeNumber(vector.tail, newAccum.toLong)
    }
  }
}

object LnUtil extends LnUtil
