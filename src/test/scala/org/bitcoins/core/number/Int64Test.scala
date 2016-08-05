package org.bitcoins.core.number

import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/15/16.
  */
class Int64Test extends FlatSpec with MustMatchers {

  "Int64" must "represent the nubmer zero" in {
    val int64 = Int64(Seq(0.toByte))
    int64.underlying must be (0)
  }
  it must "represent the number 1" in {
    val int64 = Int64(Seq(1.toByte))
    int64.underlying must be (1)
  }
  it must "represent the number -1 with 1 byte" in {
    val int64 = Int64(Seq(0xff.toByte))
    int64.underlying must be (-1)
  }

  it must "represent the number -1 with 8 bytes" in {
    val int64 = Int64(Seq(0xff.toByte,0xff.toByte,0xff.toByte,0xff.toByte,
      0xff.toByte,0xff.toByte,0xff.toByte,0xff.toByte))
    int64.underlying must be (-1)
  }
  it must "represent the Int32 max value" in {
    val int64 = Int64(Seq(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    int64.underlying must be (2147483647)
  }

  it must "represent the Int32 min value" in {
    val int64 = Int64(Seq(0x80.toByte, 0.toByte, 0.toByte, 0.toByte))
    int64.underlying must be (-2147483648L)
  }

  it must "represent the Int32 max value + 1" in {
    val int64 = Int64(Seq(0x0.toByte, 0x80.toByte, 0.toByte, 0.toByte, 0.toByte))
    int64.underlying must be (2147483648L)
  }

  it must "represent the Int32 min value - 1" in {
    val int64 = Int64(Seq(0xff.toByte, 0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    int64.underlying must be (-2147483649L)
  }

  it must "represent the minimum value for int64" in {
    val int64 = Int64(Seq(0x80.toByte,0.toByte, 0.toByte, 0.toByte,0.toByte, 0.toByte, 0.toByte, 0.toByte))
    int64.underlying must be (-9223372036854775808L)
  }

  it must "represent the maximum value for a int64" in {
    val int64 = Int64(Seq(0x7f.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte,
      0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    int64.underlying must be (9223372036854775807L)
  }

  it must "throw an exception when trying to create a Int64 out of 9 bytes or more" in {
    intercept[IllegalArgumentException] {
      Int64(Seq(0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte,0.toByte))
    }
  }

  it must "have the correct representation zero in a Int64" in {
    Int64.zero.underlying must be (0)
  }

  it must "have the correct representation for one in Int64" in {
    Int64.one.underlying must be (1)
  }

  it must "have correct number representation for the minimum number that can be stored in a Int64" in {
    Int64.min.underlying must be (-9223372036854775808L)
  }

  it must "have the correct number representation for the maximum number that can be stored in a Int64" in {
    Int64.max.underlying must be (9223372036854775807L)
  }

}
