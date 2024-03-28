package org.bitcoins.core.number

import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits.ByteVector

import scala.util.Try

/** Created by chris on 6/14/16.
  */
class UInt32Test extends BitcoinSUnitTest {

  "UInt32" must "create the number zero as an unsigned 32 bit integer" in {
    val zero = UInt32(ByteVector(0x0.toByte))
    zero.toLong must be(0)
  }

  it must "create the max number for an unsigned byte" in {
    val maxByteValue = UInt32(ByteVector(0xff.toByte))
    maxByteValue.toLong must be(255)
  }

  it must "create the number 256" in {
    val uInt32 = UInt32(ByteVector(0x01.toByte, 0x00.toByte))
    uInt32.toLong must be(256)
  }

  it must "create the number 65535" in {
    val uInt32 = UInt32(ByteVector(0xff.toByte, 0xff.toByte))
    uInt32.toLong must be(65535)
  }

  it must "create the number 65536" in {
    val uInt32 = UInt32(ByteVector(0x01.toByte, 0x0.toByte, 0x0.toByte))
    uInt32.toLong must be(65536)
  }

  it must "create the number 16777215" in {
    val uInt32 = UInt32(ByteVector(0xff.toByte, 0xff.toByte, 0xff.toByte))
    uInt32.toLong must be(16777215)
  }

  it must "create the number 16777216" in {
    val uInt32 = UInt32(ByteVector(1.toByte, 0.toByte, 0.toByte, 0.toByte))
    uInt32.toLong must be(16777216)
  }

  it must "create the number 4294967295" in {
    //this is UInt32_t's max value
    val uInt32 =
      UInt32(ByteVector(0xff.toByte, 0xff.toByte, 0xff.toByte, 0xff.toByte))
    uInt32.toLong must be(4294967295L)
    uInt32.hex must be("ffffffff")
  }

  it must "throw an exception if we try and create a UInt32 with a negative number" in {
    intercept[IllegalArgumentException] {
      UInt32(-1)
    }
  }

  it must "throw an exception if we try and create a UInt32 with more than 4 bytes" in {
    intercept[IllegalArgumentException] {
      UInt32(ByteVector(0.toByte, 0.toByte, 0.toByte, 0.toByte, 0.toByte))
    }
  }

  it must "have the correct representation for 0" in {
    UInt32.zero.toLong must be(0)
  }

  it must "have the correct representation for 1" in {
    UInt32.one.toLong must be(1)
  }

  it must "have the correct minimum number for a UInt32" in {
    UInt32.min.toLong must be(0)
  }

  it must "have the correct maximum number representation for UInt32" in {
    UInt32.max.toLong must be(4294967295L)
    UInt32.max.hex must be("ffffffff")
  }

  it must "throw an exception if we try to create a BigInt outside the range of UInt32" in {
    intercept[IllegalArgumentException] {
      UInt32(UInt32.max.toBigInt + 1)
    }
  }

  it must "serialization symmetry" in {
    forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      assert(UInt32(uInt32.hex) == uInt32)
      assert(UInt32(uInt32.hex).hex == uInt32.hex)
    }
  }

  it must "additive identity" in {
    forAll(NumberGenerator.uInt32s) { num: UInt32 =>
      assert(num + UInt32.zero == num)
    }
  }

  it must "Negative numbers in UInt32 throw an exception" in {
    forAll(NumberGenerator.negativeLongs) { num: Long =>
      val uint32 = Try(UInt32(num))
      assert(uint32.isFailure)
    }
  }

  it must "add two uint32s and get the mathematical sum of the two numbers" in {
    forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val result = num1.toLong + num2.toLong
        val res =
          if (result <= UInt32.max.toLong) num1 + num2 == UInt32(result.toLong)
          else Try(num1 + num2).isFailure
        assert(res)
    }
  }

  it must "subtractive identity" in {
    forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      assert(uInt32 - UInt32.zero == uInt32)
    }
  }

  it must "subtract a uint32 from another uint32 and get the correct result" in {
    forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val result = num1.toLong - num2.toLong
        val res =
          if (result >= 0) num1 - num2 == UInt32(result)
          else Try(num1 - num2).isFailure
        assert(res)
    }
  }

  it must "multiplying by zero gives us zero" in {
    forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      assert(uInt32 * UInt32.zero == UInt32.zero)
    }
  }

  it must "multiplicative identity" in {
    forAll(NumberGenerator.uInt32s) { uInt32: UInt32 =>
      assert(uInt32 * UInt32.one == uInt32)
    }
  }

  it must "multiply two UInt32s" in {
    forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val bigInt1 = num1.toBigInt
        val bigInt2 = num2.toBigInt
        val result = if (bigInt1 * bigInt2 <= UInt32.max.toLong) {
          num1 * num2 ==
            UInt32(num1.toLong * num2.toLong)
        } else Try(num1 * num2).isFailure
        assert(result)
    }
  }

  it must "< & >=" in {
    forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val result =
          if (num1.toLong < num2.toLong) num1 < num2
          else num1 >= num2
        assert(result)
    }
  }

  it must "<= & >" in {
    forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val result =
          if (num1.toLong <= num2.toLong) num1 <= num2
          else num1 > num2
        assert(result)
    }
  }

  it must "== & !=" in {
    forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        val result =
          if (num1.toLong == num2.toLong) num1 == num2
          else num1 != num2
        assert(result)
    }
  }

  it must "|" in {
    forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        assert(UInt32(num1.toLong | num2.toLong) == (num1 | num2))
    }
  }

  it must "&" in {
    forAll(NumberGenerator.uInt32s, NumberGenerator.uInt32s) {
      (num1: UInt32, num2: UInt32) =>
        assert(UInt32(num1.toLong & num2.toLong) == (num1 & num2))
    }
  }

  it must "<<" in {
    forAll(NumberGenerator.uInt32s, Gen.choose(0, 32)) { case (u32, shift) =>
      val r = Try(u32 << shift)
      val expected = (u32.toLong << shift) & 0xffffffffL
      val result = if (r.isSuccess && expected <= UInt32.max.toLong) {
        r.get == UInt32(expected)
      } else {
        r.isFailure
      }
      assert(result)
    }
  }

  it must ">>" in {
    forAll(NumberGenerator.uInt32s, Gen.choose(0, 100)) { case (u32, shift) =>
      val r = u32 >> shift
      val expected = if (shift >= 64) 0 else u32.toLong >> shift
      assert(r == UInt32(expected))
    }
  }
}
