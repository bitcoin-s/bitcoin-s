package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.GetCompactFiltersMessage
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class RawGetCompactFiltersMessageSerializerTest extends BitcoinSUnitTest {

  it must "parse a message" in {
    // cribbed from a P2P log dump with Bitcoin-S node
    val bytes = hex"00" ++ //type
      hex"1d0c0000" ++ // start height
      hex"83abc1c5f4d8c065208b3c0b5f8b61a373c79b647b0421c56ee50c6f0ddd2917" // stop hash

    val message = GetCompactFiltersMessage.fromBytes(bytes)

    assert(message.filterType == FilterType.Basic)
    assert(message.startHeight.toInt == 3101)
    assert(
      message.stopHash == DoubleSha256Digest.fromHex(
        "83abc1c5f4d8c065208b3c0b5f8b61a373c79b647b0421c56ee50c6f0ddd2917"))
  }

  it must "have serialization symmetry" in {
    val bytes = hex"00" ++ //type
      hex"1d0c0000" ++ // start height
      hex"83abc1c5f4d8c065208b3c0b5f8b61a373c79b647b0421c56ee50c6f0ddd2917" // stop hash

    val message = GetCompactFiltersMessage.fromBytes(bytes)

    assert(bytes == message.bytes)

    val anotherMessage = GetCompactFiltersMessage(
      0x0180,
      DoubleSha256Digest.fromHex(
        "8000000000000000000000000000000000000000000000000000000000000001"))

    val anotherBytes = anotherMessage.bytes

    assert(anotherMessage == GetCompactFiltersMessage.fromBytes(anotherBytes))
  }

}
