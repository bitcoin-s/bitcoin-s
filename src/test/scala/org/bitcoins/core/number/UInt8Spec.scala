package org.bitcoins.core.number

import org.bitcoins.core.gen.NumberGenerator
import org.scalacheck.{Prop, Properties}

class UInt8Spec extends Properties("UInt8Spec") {

  property("convert uint8 -> byte -> uint8") = {
    Prop.forAll(NumberGenerator.uInt8) { case u8: UInt8 =>
        UInt8(UInt8.toByte(u8)) == u8
    }
  }
}
