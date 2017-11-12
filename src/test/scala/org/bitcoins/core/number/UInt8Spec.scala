package org.bitcoins.core.number

import org.bitcoins.core.gen.NumberGenerator
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.{Gen, Prop, Properties}

import scala.util.Try

class UInt8Spec extends Properties("UInt8Spec") {
  private val logger = BitcoinSLogger.logger
  property("convert uint8 -> byte -> uint8") = {
    Prop.forAll(NumberGenerator.uInt8) { case u8: UInt8 =>
        UInt8(UInt8.toByte(u8)) == u8
    }
  }

  property("serialization symmetry") = {
    Prop.forAll(NumberGenerator.uInt8) { u8 =>
      UInt8(u8.hex) == u8
    }
  }

  property("<<") = {
    Prop.forAllNoShrink(NumberGenerator.uInt8, Gen.choose(0,8)) { case (u8: UInt8, shift: Int) =>
      val r = Try(u8 << shift)
      val expected = (u8.underlying << shift) & 0xffL
      if (expected <= UInt8.max.underlying) {
        r.get == UInt8(expected.toShort)
      } else {
        r.isFailure
      }
    }
  }

  property(">>") = {
    Prop.forAllNoShrink(NumberGenerator.uInt8,Gen.choose(0,100)) { case (u8: UInt8, shift: Int) =>
      val r = (u8 >> shift)
      val expected = u8.underlying >> shift
      r == UInt8(expected.toShort)

    }
  }
}
