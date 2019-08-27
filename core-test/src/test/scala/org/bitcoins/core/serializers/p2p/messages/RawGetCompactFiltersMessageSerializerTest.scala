package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.GetCompactFiltersMessage
import org.bitcoins.testkit.util.BitcoinSUnitTest

class RawGetCompactFiltersMessageSerializerTest extends BitcoinSUnitTest {

  it must "write a message" ignore {
    val message = GetCompactFiltersMessage(
      filterType = FilterType.Basic,
      startHeight = UInt32(900),
      stopHash = DoubleSha256Digest.fromHex("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"))

    val bytes = message.bytes

    bytes must be(empty)
  }

}
