package org.bitcoins.core.number

import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import scala.util.Try

/** Created by chris on 6/15/16.
  */
class Int32Test extends BitcoinSUnitTest {

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

  it must "convert to UInt32" in {
    Int32.zero.toUInt32 must be(UInt32.zero)
    Int32.negOne.toUInt32 must be(UInt32.max)
    Int32.two.toUInt32 must be(UInt32.two)
  }

  it must "Serialization symmetry" in {
    forAll(NumberGenerator.int32s) { int32: Int32 =>
      assert(Int32(int32.hex) == int32)
    }
  }

  it must "Additive identity" in {
    forAll(NumberGenerator.int32s) { int32: Int32 =>
      assert(int32 + Int32.zero == int32)
    }
  }

  it must "Add two arbitrary int32s" in {
    forAll(NumberGenerator.int32s, NumberGenerator.int32s) {
      (num1: Int32, num2: Int32) =>
        val result = num1.toLong + num2.toLong
        val res =
          if (result <= Int32.max.toLong && result >= Int32.min.toLong)
            num1 + num2 == Int32(result)
          else Try(num1 + num2).isFailure
        assert(res)
    }
  }

  it must "Subtractive identity" in {
    forAll(NumberGenerator.int32s) { int32: Int32 =>
      assert(int32 - Int32.zero == int32)
    }
  }

  it must "Subtract two arbitrary int32s" in {
    forAll(NumberGenerator.int32s, NumberGenerator.int32s) {
      (num1: Int32, num2: Int32) =>
        val result = num1.toLong - num2.toLong
        val res =
          if (result >= Int32.min.toLong && result <= Int32.max.toLong)
            num1 - num2 == Int32(result)
          else Try(num1 - num2).isFailure
        assert(res)
    }
  }

  it must "Multiplying by zero" in {
    forAll(NumberGenerator.int32s) { int32: Int32 =>
      assert(int32 * Int32.zero == Int32.zero)
    }
  }

  it must "Multiplicative identity" in {
    forAll(NumberGenerator.int32s) { int32: Int32 =>
      assert(int32 * Int32.one == int32)
    }
  }

  it must "Multiply two int32s" in {
    forAll(NumberGenerator.int32s, NumberGenerator.int32s) {
      (num1: Int32, num2: Int32) =>
        val result = num1.toLong * num2.toLong
        val res =
          if (result >= Int32.min.toLong && result <= Int32.max.toLong)
            num1 * num2 == Int32(result.toInt)
          else Try(num1 * num2).isFailure
        assert(res)
    }
  }

  it must "<= & >" in {
    forAll(NumberGenerator.int32s, NumberGenerator.int32s) {
      (num1: Int32, num2: Int32) =>
        val result =
          if (num1.toLong <= num2.toLong) num1 <= num2
          else num1 > num2
        assert(result)
    }
  }

  it must "< & =>" in {
    forAll(NumberGenerator.int32s, NumberGenerator.int32s) {
      (num1: Int32, num2: Int32) =>
        val result =
          if (num1.toLong < num2.toLong) num1 < num2
          else num1 >= num2
        assert(result)
    }
  }

  it must "== & !=" in {
    forAll(NumberGenerator.int32s, NumberGenerator.int32s) {
      (num1: Int32, num2: Int32) =>
        val result =
          if (num1.toLong == num2.toLong) num1 == num2
          else num1 != num2
        assert(result)
    }
  }

  it must "|" in {
    forAll(NumberGenerator.int32s, NumberGenerator.int32s) {
      (num1: Int32, num2: Int32) =>
        assert(Int32(num1.toLong | num2.toLong) == (num1 | num2))
    }
  }

  it must "&" in {
    forAll(NumberGenerator.int32s, NumberGenerator.int32s) {
      (num1: Int32, num2: Int32) =>
        assert(Int32(num1.toLong & num2.toLong) == (num1 & num2))
    }
  }

  it must "negation" in {
    forAll(NumberGenerator.int32s) { int32 =>
      assert(-int32 == Int32(-int32.toLong))
    }
  }
}
