package org.bitcoins.core.number

import org.scalatest.{FlatSpec, MustMatchers}
import scodec.bits.ByteVector

/**
  * Created by chris on 6/15/16.
  */
class Int32Test extends FlatSpec with MustMatchers {

  "Int32" must "create the number zero" in {
    val int32 = Int32(ByteVector.low(1))
    int32.toInt must be(0)
  }

  it must "represent the number -1" in {
    val int32 = Int32(ByteVector(0xff.toByte))
    int32.toInt must be(-1)
  }

  it must "represent the number -1 with 4 bytes" in {
    val int32 =
      Int32(ByteVector(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    int32.toInt must be(-1)
  }

  it must "create the max number for a single byte" in {
    val int32 = Int32(ByteVector(0x7f.toByte))
    int32.toInt must be(127)
  }

  it must "create the min number for a single byte" in {
    val int32 = Int32(ByteVector(0x80.toByte))
    int32.toInt must be(-128)
  }

  it must "create the max number for an Int32" in {
    val int32 =
      Int32(ByteVector(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    int32.toInt must be(2147483647)
  }

  it must "create the minimum number for an Int32" in {
    val int32 = Int32(ByteVector(0x80.toByte, 0.toByte, 0.toByte, 0.toByte))
    int32.toInt must be(-2147483648)
  }

  it must "throw an exception if we try and create an Int32 with more than 4 bytes" in {
    intercept[IllegalArgumentException] {
      Int32(ByteVector(0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte))
    }
  }

  it must "have the correct representation for 0" in {
    Int32.zero.toInt must be(0)
  }

  it must "have the correct representation for 1" in {
    Int32.one.toInt must be(1)
  }

  it must "have the correct minimum number representation" in {
    Int32.min.toInt must be(-2147483648)
  }

  it must "have the correct maximum number representation" in {
    Int32.max.toInt must be(2147483647)
  }
}
