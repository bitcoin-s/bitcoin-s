package org.bitcoins.core.p2p

import org.bitcoins.core.bloom.{BloomFilter, BloomUpdateAll}
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.testkit.core.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class FilterLoadMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.filterLoadMessage) { filterMsg =>
      assert(FilterLoadMessage(filterMsg.hex) == filterMsg)
    }
  }

  it must "throw on too large filters" in {
    intercept[IllegalArgumentException] {
      val size = CompactSizeUInt(UInt64(36001))
      val data = ByteVector.empty
      val hashFuncs = UInt32.one
      val tweak = UInt32.zero
      val flags = BloomUpdateAll
      val bloom = BloomFilter(filterSize = size,
                              data = data,
                              hashFuncs = hashFuncs,
                              tweak = tweak,
                              flags = flags)
      FilterLoadMessage(bloom)
    }
  }

  it must "throw on too many hashfuncs" in {
    intercept[IllegalArgumentException] {
      val size = CompactSizeUInt(UInt64(36000))
      val data = ByteVector.empty
      val hashFuncs = UInt32(51)
      val tweak = UInt32.zero
      val flags = BloomUpdateAll
      val bloom = BloomFilter(filterSize = size,
                              data = data,
                              hashFuncs = hashFuncs,
                              tweak = tweak,
                              flags = flags)
      FilterLoadMessage(bloom)
    }
  }

  it must "throw on size discrepancy" in {
    intercept[IllegalArgumentException] {
      val size = CompactSizeUInt(UInt64(36000))
      val data = ByteVector.empty
      val hashFuncs = UInt32.one
      val tweak = UInt32.zero
      val flags = BloomUpdateAll
      val bloom = BloomFilter(filterSize = size,
                              data = data,
                              hashFuncs = hashFuncs,
                              tweak = tweak,
                              flags = flags)
      FilterLoadMessage(bloom)
    }
  }
}
