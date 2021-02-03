package org.bitcoins.core.number

import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits.ByteVector

import scala.util.Try

class UInt16Test extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "UInt16"

  it must "create the number zero as an unsigned 16 bit integer" in {
    val zero = UInt16(ByteVector(0x0.toByte))
    assert(zero.toInt == 0)
  }

  it must "create the max number for an unsigned byte" in {
    val maxByteValue = UInt16(ByteVector(0xff.toByte))
    assert(maxByteValue.toInt == 255)
  }

  it must "create the number 256" in {
    val uInt16 = UInt16(ByteVector(0x01.toByte, 0x00.toByte))
    assert(uInt16.toInt == 256)
  }

  it must "create the number 65535" in {
    val uInt16 = UInt16(ByteVector(0xff.toByte, 0xff.toByte))
    assert(uInt16.toInt == 65535)
  }

  it must "have the correct maximum number representation for UInt16" in {
    assert(UInt16.max.toInt == 65535)
    assert(UInt16.max.hex == "ffff")
  }

  it must "fail to create the number 65536" in {
    assertThrows[IllegalArgumentException] {
      UInt16(ByteVector(0x01.toByte, 0x0.toByte, 0x0.toByte))
    }

    assertThrows[IllegalArgumentException] {
      UInt16(65536)
    }
  }

  it must "throw an exception if we try and create a UInt16 with a negative number" in {
    assertThrows[IllegalArgumentException] {
      UInt16(-1)
    }
  }

  it must "throw an exception if we try and create a UInt16 with more than 2 bytes" in {
    assertThrows[IllegalArgumentException] {
      UInt16(ByteVector(0.toByte, 0.toByte, 0.toByte))
    }
  }

  it must "have the correct representation for 0" in {
    assert(UInt16.zero.toInt == 0)
  }

  it must "have the correct representation for 1" in {
    assert(UInt16.one.toInt == 1)
  }

  it must "have the correct minimum number for a UInt16" in {
    assert(UInt16.min.toInt == 0)
  }

  it must "have serialization symmetry" in {
    forAll(NumberGenerator.uInt16) { uInt16: UInt16 =>
      assert(UInt16(uInt16.hex) == uInt16)
      assert(UInt16(uInt16.hex).hex == uInt16.hex)
    }
  }

  it must "add zero correctly" in {
    forAll(NumberGenerator.uInt16) { num: UInt16 =>
      assert(num + UInt16.zero == num)
    }
  }

  it must "Negative numbers in UInt16 throw an exception" in {
    forAll(NumberGenerator.negativeInts) { num =>
      val uint16 = Try(UInt16(num))
      assert(uint16.isFailure)
    }
  }

  it must "add two uint16s and get the mathematical sum of the two numbers" in {
    forAll(NumberGenerator.uInt16, NumberGenerator.uInt16) {
      (num1: UInt16, num2: UInt16) =>
        val result = num1.toInt + num2.toInt
        if (result <= UInt16.max.toInt) {
          assert(num1 + num2 == UInt16(result.toInt))
        } else {
          assert(Try(num1 + num2).isFailure)
        }
    }
  }

  it must "subtract zero correctly" in {
    forAll(NumberGenerator.uInt16) { uInt16: UInt16 =>
      assert(uInt16 - UInt16.zero == uInt16)
    }
  }

  it must "subtract from zero correctly" in {
    forAll(NumberGenerator.uInt16) { num =>
      if (num == UInt16.zero) {
        assert(UInt16.zero - num == UInt16.zero)
      } else {
        assert(Try(UInt16.zero - num).isFailure)
      }
    }
  }

  it must "subtract a uint16 from another uint16 and get the correct result" in {
    forAll(NumberGenerator.uInt16, NumberGenerator.uInt16) {
      (num1: UInt16, num2: UInt16) =>
        val result = num1.toInt - num2.toInt
        if (result >= 0) {
          assert(num1 - num2 == UInt16(result))
        } else {
          assert(Try(num1 - num2).isFailure)
        }
    }
  }

  it must "multiplying by zero correctly" in {
    forAll(NumberGenerator.uInt16) { uInt16: UInt16 =>
      assert(uInt16 * UInt16.zero == UInt16.zero)
    }
  }

  it must "multiply by one correctly" in {
    forAll(NumberGenerator.uInt16) { uInt16: UInt16 =>
      assert(uInt16 * UInt16.one == uInt16)
    }
  }

  it must "multiply two UInt16s correctly" in {
    forAll(NumberGenerator.uInt16, NumberGenerator.uInt16) {
      (num1: UInt16, num2: UInt16) =>
        val bigInt1 = num1.toBigInt
        val bigInt2 = num2.toBigInt
        if (bigInt1 * bigInt2 <= UInt16.max.toInt) {
          assert(
            num1 * num2 ==
              UInt16(num1.toInt * num2.toInt))
        } else {
          assert(Try(num1 * num2).isFailure)
        }
    }
  }

  it must "compare UInt16s correctly" in {
    forAll(NumberGenerator.uInt16, NumberGenerator.uInt16) {
      (num1: UInt16, num2: UInt16) =>
        if (num1.toInt < num2.toInt) assert(num1 < num2)
        else assert(num1 >= num2)

        if (num1.toInt <= num2.toInt) assert(num1 <= num2)
        else assert(num1 > num2)

        if (num1.toInt == num2.toInt) assert(num1 == num2)
        else assert(num1 != num2)
    }
  }

  it must "| correctly" in {
    forAll(NumberGenerator.uInt16, NumberGenerator.uInt16) {
      (num1: UInt16, num2: UInt16) =>
        assert(UInt16(num1.toInt | num2.toInt) == (num1 | num2))
    }
  }

  it must "& correctly" in {
    forAll(NumberGenerator.uInt16, NumberGenerator.uInt16) {
      (num1: UInt16, num2: UInt16) =>
        assert(UInt16(num1.toInt & num2.toInt) == (num1 & num2))
    }
  }

  it must "<< correctly" in {
    forAll(NumberGenerator.uInt16, Gen.choose(0, 16)) { case (u16, shift) =>
      val r = Try(u16 << shift)
      val expected = (u16.toInt << shift) & 0xffff
      if (r.isSuccess && expected <= UInt16.max.toInt) {
        assert(r.get == UInt16(expected))
      } else {
        assert(r.isFailure)
      }
    }
  }

  it must ">> correctly" in {
    forAll(NumberGenerator.uInt16, Gen.choose(0, 100)) { case (u16, shift) =>
      val r = u16 >> shift
      val expected = if (shift >= 32) 0 else u16.toInt >> shift
      assert(r == UInt16(expected))
    }
  }
}
