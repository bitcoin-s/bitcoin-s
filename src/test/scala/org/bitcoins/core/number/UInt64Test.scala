package org.bitcoins.core.number

import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/15/16.
  */
class UInt64Test extends FlatSpec with MustMatchers {

  "UInt64" must "hold the number 0" in {
    val uInt64 = UInt64(Seq(0.toByte))
    uInt64.underlying must be (0)
  }

  it must "hold the max for a uint32_t" in {
    //this is UInt32_t's max value
    val uInt64 = UInt64(Seq(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    uInt64.underlying must be (4294967295L)
  }

  it must "hold the max for uint32_t + 1" in {
    val uInt64 = UInt64(Seq(1.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte))
    uInt64.underlying must be (4294967296L)
  }

  it must "hold the max number for uint64_t" in {
    val uInt64 = UInt64(Seq(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
      0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    uInt64.underlying must be (BigInt("18446744073709551615"))
  }

  it must "throw an exception if we try and create a number larger than 8 bytes" in {
    intercept[IllegalArgumentException] {
      UInt64(Seq(1.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte))
    }
  }
}
