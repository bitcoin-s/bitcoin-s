package org.bitcoins.core.number

import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits.ByteVector

import scala.util.Try

class UInt8Test extends BitcoinSUnitTest {

  "UInt8" must "convert a byte to a UInt8 correctly" in {
    UInt8.toUInt8(0.toByte) must be(UInt8.zero)
    UInt8.toUInt8(1.toByte) must be(UInt8.one)
    UInt8.toUInt8(255.toByte) must be(UInt8(255.toShort))
  }

  it must "throw an exception if we try and create an UInt8 with more than 1 bytes" in {
    intercept[IllegalArgumentException] {
      UInt8(ByteVector(0.toByte, 0.toByte))
    }
  }

  it must "convert uint8 -> byte -> uint8" in {
    forAll(NumberGenerator.uInt8) { case u8: UInt8 =>
      UInt8(UInt8.toByte(u8)) == u8
    }
  }

  it must "serialization symmetry" in {
    forAll(NumberGenerator.uInt8) { u8 =>
      UInt8(u8.hex) == u8
    }
  }

  it must "<<" in {
    forAll(NumberGenerator.uInt8, Gen.choose(0, 8)) {
      case (u8: UInt8, shift: Int) =>
        val r = Try(u8 << shift)
        val expected = (u8.toLong << shift) & 0xffL
        if (expected <= UInt8.max.toLong) {
          r.get == UInt8(expected.toShort)
        } else {
          r.isFailure
        }
    }
  }

  it must ">>" in {
    forAll(NumberGenerator.uInt8, Gen.choose(0, 100)) {
      case (u8: UInt8, shift: Int) =>
        val r = u8 >> shift
        val expected =
          if (shift > 31) UInt8.zero else UInt8((u8.toLong >> shift).toShort)
        r == expected
    }
  }

  it must "do basic xor" in {
    assert(UInt8.zero.^(UInt8.zero) == UInt8.zero)
    assert(UInt8.one.^(UInt8.zero) == UInt8.one)
    assert(UInt8.max.^(UInt8.zero) == UInt8.max)
  }

}
