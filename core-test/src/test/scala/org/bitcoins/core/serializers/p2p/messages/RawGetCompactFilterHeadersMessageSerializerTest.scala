package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p.GetCompactFilterHeadersMessage
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class RawGetCompactFilterHeadersMessageSerializerTest extends BitcoinSUnitTest {

  it must "parse a message" in {
    // cribbed from a P2P log dump with Bitcoin-S node
    val bytes = hex"00" ++ // type
      hex"d0070000" ++ // start height
      hex"6f0ee334fbba823804e14042c33bc5dfa5126e5076d8dcff02d4a045f266f427" // stop hash

    val message = GetCompactFilterHeadersMessage.fromBytes(bytes)

    assert(message.filterType == FilterType.Basic)
    assert(message.startHeight.toInt == 2000)
    assert(
      message.stopHash == DoubleSha256Digest.fromHex(
        "6f0ee334fbba823804e14042c33bc5dfa5126e5076d8dcff02d4a045f266f427"))
  }

  it must "have serialization symmetry" in {
    val bytes = hex"00" ++ // type
      hex"d0070000" ++ // start height
      hex"6f0ee334fbba823804e14042c33bc5dfa5126e5076d8dcff02d4a045f266f427" // stop hash

    val message = GetCompactFilterHeadersMessage.fromBytes(bytes)

    assert(bytes == message.bytes)

    val anotherMessage = GetCompactFilterHeadersMessage(
      0x0180,
      DoubleSha256Digest.fromHex(
        "8000000000000000000000000000000000000000000000000000000000000001"))

    val anotherBytes = anotherMessage.bytes

    assert(
      anotherMessage == GetCompactFilterHeadersMessage.fromBytes(anotherBytes))
  }

}
