package org.bitcoins.core.number

import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

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

}
