package org.bitcoins.core.protocol.ln

import org.bitcoins.testkit.core.gen.NumberGenerator
import org.bitcoins.core.number.{UInt5, UInt64}
import org.bitcoins.core.protocol.ln.util.LnUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

class LnUtilTest extends BitcoinSUnitTest {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "LnUtil"

  it must "encode / decode a number correctly to bech32" in {

    forAll(NumberGenerator.uInt64) {
      case u64: UInt64 =>
        val encoded = LnUtil.encodeNumber(u64.toBigInt)
        val decoded = LnUtil.decodeNumber(encoded)

        assert(u64 == UInt64(decoded))
    }
  }

  it must "encode the data length correctly for empty payload" in {
    val empty = LnUtil.createDataLength(List.empty)

    empty must be(List(UInt5.zero, UInt5.zero))
  }

  it must "encode the property data length for an arbitrary payload" in {
    forAll(NumberGenerator.uInt5s) {
      case u5s =>
        val dataLen = LnUtil.createDataLength(u5s.toList)

        val decodedDataLen = LnUtil.decodeDataLength(dataLen)

        assert(decodedDataLen == u5s.length)

    }
  }
}
