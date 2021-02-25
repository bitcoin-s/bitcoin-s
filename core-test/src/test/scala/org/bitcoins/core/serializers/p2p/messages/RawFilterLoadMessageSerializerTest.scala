package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.bloom.BloomUpdateNone
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 7/20/16.
  */
class RawFilterLoadMessageSerializerTest extends BitcoinSUnitTest {

  "RawFilterLoadMessageSerializer" must "deserialize and serialize a filter load message" in {
    //example from the bitcoin developer reference
    //https://bitcoin.org/en/developer-reference#filterload
    val hex = "02b50f0b0000000000000000"

    val filterLoadMsg = RawFilterLoadMessageSerializer.read(hex)
    filterLoadMsg.bloomFilter.filterSize must be(CompactSizeUInt(UInt64(2)))
    BytesUtil.encodeHex(filterLoadMsg.bloomFilter.data) must be("b50f")
    filterLoadMsg.bloomFilter.hashFuncs must be(UInt32(11))
    filterLoadMsg.bloomFilter.tweak must be(UInt32.zero)
    filterLoadMsg.bloomFilter.flags must be(BloomUpdateNone)

    RawFilterLoadMessageSerializer.write(filterLoadMsg).toHex must be(hex)

  }
}
