package org.bitcoins.core.number

import org.scalatest.{FlatSpec, MustMatchers}

class UInt8Test extends FlatSpec with MustMatchers {

  "UInt8" must "convert a byte to a UInt8 correctly" in {
    UInt8.toUInt8(0.toByte) must be (UInt8.zero)
    UInt8.toUInt8(1.toByte) must be (UInt8.one)
    UInt8.toUInt8(255.toByte) must be (UInt8(255.toShort))
  }

}
