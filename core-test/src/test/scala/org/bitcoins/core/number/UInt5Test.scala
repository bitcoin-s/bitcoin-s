package org.bitcoins.core.number

import org.bitcoins.core.gen.NumberGenerator
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, MustMatchers }
import org.slf4j.LoggerFactory

class UInt5Test extends FlatSpec with MustMatchers with PropertyChecks {

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  behavior of "UInt5"

  it must "convert a byte to a UInt5 correctly" in {
    UInt5.fromByte(0.toByte) must be(UInt5.zero)
    UInt5(1.toByte) must be(UInt5.one)

    UInt5(31.toByte) must be(UInt5.max)
  }

  it must "not allow negative numbers" in {
    intercept[IllegalArgumentException] {
      UInt5(-1)
    }
  }

  it must "not allow numbers more than 32" in {
    intercept[IllegalArgumentException] {
      UInt5(32)
    }
  }

  it must "have serialization symmetry" in {
    forAll(NumberGenerator.uInt5) { u5 =>
      val u52 = UInt5.fromHex(u5.hex)
      u52 == u5
    }
  }

  it must "uint5 -> byte -> uint5" in {
    forAll(NumberGenerator.uInt5) { u5 =>
      val byte = u5.byte
      UInt5.fromByte(byte) == u5
    }
  }

  it must "uint5 -> uint8 -> uint5" in {
    forAll(NumberGenerator.uInt5) { u5 =>
      val u8 = u5.toUInt8
      u8.toUInt5 == u5
    }
  }
}
