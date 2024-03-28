package org.bitcoins.core.number

import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

import scala.util.Try

/** Created by chris on 6/15/16.
  */
class UInt64Test extends BitcoinSUnitTest {

  "UInt64" must "hold the number 0" in {
    val uInt64 = UInt64(ByteVector.low(1))
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
    val uInt64 =
      UInt64(ByteVector(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    uInt64.toBigInt must be(4294967295L)
  }

  it must "hold the max for uint32_t + 1" in {
    val uInt64 =
      UInt64(ByteVector(1.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte))
    uInt64.toBigInt must be(4294967296L)
  }

  it must "hold the max number for uint64_t" in {
    val uInt64 = UInt64(
      ByteVector(0xff.toByte,
                 0xff.toByte,
                 0xff.toByte,
                 0xff.toByte,
                 0xff.toByte,
                 0xff.toByte,
                 0xff.toByte,
                 0xff.toByte))
    uInt64.toBigInt must be(BigInt("18446744073709551615"))
    uInt64.hex must be("ffffffffffffffff")
  }

  it must "throw an exception if we try and create a number larger than 8 bytes" in {
    intercept[IllegalArgumentException] {
      UInt64(
        ByteVector(1.toByte,
                   0.toByte,
                   0.toByte,
                   0.toByte,
                   0.toByte,
                   0.toByte,
                   0.toByte,
                   0.toByte,
                   0.toByte))
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

  it must "Serialization symmetry" in {
    forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      assert(UInt64(uInt64.hex) == uInt64)
      assert(UInt64(uInt64.hex).hex == uInt64.hex)
    }
  }

  it must "additive identity" in {
    forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      val result =
        if (uInt64.toBigInt <= UInt64.max.toBigInt)
          uInt64 + UInt64.zero == UInt64(uInt64.toBigInt)
        else uInt64 + UInt64.zero == uInt64
      assert(result)
    }
  }

  it must "add two arbitrary uInt64s" in {
    forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) {
      (num1: UInt64, num2: UInt64) =>
        val result: BigInt = num1.toBigInt + num2.toBigInt
        val res =
          if (result <= UInt64.max.toBigInt) num1 + num2 == UInt64(result)
          else Try(num1 + num2).isFailure
        assert(res)
    }
  }

  it must "subtractive identity" in {
    forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      val result =
        if (uInt64.toBigInt <= UInt64.max.toBigInt)
          uInt64 - UInt64.zero == UInt64(uInt64.toBigInt)
        else uInt64 - UInt64.zero == uInt64
      assert(result)
    }
  }

  it must "subtract a uint64 from a uint64" in {
    forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) {
      (num1: UInt64, num2: UInt64) =>
        val result = num1.toBigInt - num2.toBigInt
        val res =
          if (result < 0) Try(num1 - num2).isFailure
          else num1 - num2 == UInt64(result)
        assert(res)
    }
  }

  it must "multiplying by zero" in {
    forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      assert(uInt64 * UInt64.zero == UInt64.zero)
    }
  }

  it must "multiplicative identity" in {
    forAll(NumberGenerator.uInt64s) { uInt64: UInt64 =>
      val result =
        if (uInt64 == UInt64.zero) uInt64 * UInt64.one == UInt64.zero
        else uInt64 * UInt64.one == uInt64
      assert(result)
    }
  }

  it must "multiply two uInt64s" in {
    forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) {
      (num1: UInt64, num2: UInt64) =>
        val result = num1.toBigInt * num2.toBigInt
        val res =
          if (result <= UInt64.max.toBigInt) num1 * num2 == UInt64(result)
          else Try(num1 * num2).isFailure
        assert(res)
    }
  }

  it must "< & >= for uInt64s" in {
    forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) {
      (num1: UInt64, num2: UInt64) =>
        val result =
          if (num1.toBigInt < num2.toBigInt) num1 < num2
          else num1 >= num2
        assert(result)
    }
  }

  it must "<= & > with two uInt64s" in {
    forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) {
      (num1: UInt64, num2: UInt64) =>
        val result =
          if (num1.toBigInt <= num2.toBigInt) num1 <= num2
          else num1 > num2
        assert(result)
    }
  }
  it must "== & != for two UInt64s" in {
    forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) {
      (num1: UInt64, num2: UInt64) =>
        val result =
          if (num1.toBigInt == num2.toBigInt) num1 == num2
          else num1 != num2
        assert(result)
    }
  }
  it must "|" in {
    forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) {
      (num1: UInt64, num2: UInt64) =>
        assert(UInt64(num1.toBigInt | num2.toBigInt) == (num1 | num2))
    }
  }

  it must "&" in {
    forAll(NumberGenerator.uInt64s, NumberGenerator.uInt64s) {
      (num1: UInt64, num2: UInt64) =>
        assert(UInt64(num1.toBigInt & num2.toBigInt) == (num1 & num2))
    }
  }

  it must "^" in {
    forAll(NumberGenerator.uInt64s) { u64: UInt64 =>
      assert(u64.^(UInt64.zero) == u64)
      assert(u64.^(u64) == UInt64.zero)
    }
  }
}
