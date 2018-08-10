package org.bitcoins.core.number

import org.scalatest.{ FlatSpec, MustMatchers }
import scodec.bits.ByteVector

/**
 * Created by chris on 6/15/16.
 */
class UInt64Test extends FlatSpec with MustMatchers {

  "UInt64" must "hold the number 0" in {
    val uInt64 = UInt64(scodec.bits.ByteVector.low(1))
    uInt64.hex must be("0000000000000000")
    uInt64.toBigInt must be(0)
  }

  it must "encode the number 1" in {
    val uInt64 = UInt64(1)
    uInt64.toBigInt must be(1)
    uInt64.hex must be("0000000000000001")
  }

  it must "hold the max for a uint32_t" in {
    //this is UInt32_t's max value
    val uInt64 = UInt64(scodec.bits.ByteVector(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    uInt64.toBigInt must be(4294967295L)
  }

  it must "hold the max for uint32_t + 1" in {
    val uInt64 = UInt64(ByteVector(1.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte))
    uInt64.toBigInt must be(4294967296L)
  }

  it must "hold the max number for uint64_t" in {
    val uInt64 = UInt64(scodec.bits.ByteVector(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
      0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    uInt64.toBigInt must be(BigInt("18446744073709551615"))
    uInt64.hex must be("ffffffffffffffff")
  }

  it must "throw an exception if we try and create a number larger than 8 bytes" in {
    intercept[IllegalArgumentException] {
      UInt64(ByteVector(1.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte))
    }
  }

  it must "have the correct representation for 0" in {
    UInt64.zero.toBigInt must be(0)
  }

  it must "have the correct representation for 1" in {
    UInt64.one.toBigInt must be(1)
  }

  it must "have the correct min number for a UInt64" in {
    UInt64.min.toBigInt must be(0)
  }

  it must "have the correct max number for a UInt64" in {
    UInt64.max.toBigInt must be(BigInt("18446744073709551615"))
  }

  it must "throw an exception if we try to create a BigInt outside the range of UInt64" in {
    intercept[IllegalArgumentException] {
      UInt64(UInt64.max.toBigInt + 1)
    }
  }
}
