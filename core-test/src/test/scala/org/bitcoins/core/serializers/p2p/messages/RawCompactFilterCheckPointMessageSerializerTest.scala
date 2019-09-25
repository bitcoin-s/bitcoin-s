package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.p2p.CompactFilterCheckPointMessage
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class RawCompactFilterCheckPointMessageSerializerTest extends BitcoinSUnitTest {
  it must "parse a message" in {
    // cribbed from a P2P log dump with Bitcoin-S node
    val bytes = hex"00" ++ // type
      hex"6f0ee334fbba823804e14042c33bc5dfa5126e5076d8dcff02d4a045f266f427" ++ // stop hash
      hex"03" ++ // num filter headers
      hex"93daaed620ff44fb7a760860ee8084f7f8c722d6c7ea9d1e1a35059253f876e6" ++ // filter headers
      hex"2c2faad9d5e25594914772dc815e157debf385cbe5de5a0aea59d15af42b19ad" ++
      hex"dd9cc1baf1453d682d27958c0f64c97a69249d655151c3b25b1ef1a993ec4f4f"
    val message = CompactFilterCheckPointMessage.fromBytes(bytes)

    assert(message.filterType == FilterType.Basic)
    assert(
      message.stopHash == DoubleSha256Digest.fromHex(
        "6f0ee334fbba823804e14042c33bc5dfa5126e5076d8dcff02d4a045f266f427"))
    assert(
      message.filterHeaders == Vector(
        DoubleSha256Digest.fromHex(
          "93daaed620ff44fb7a760860ee8084f7f8c722d6c7ea9d1e1a35059253f876e6"),
        DoubleSha256Digest.fromHex(
          "2c2faad9d5e25594914772dc815e157debf385cbe5de5a0aea59d15af42b19ad"),
        DoubleSha256Digest.fromHex(
          "dd9cc1baf1453d682d27958c0f64c97a69249d655151c3b25b1ef1a993ec4f4f")
      ))
  }

  it must "have serialization symmetry" in {
    val bytes = hex"00" ++
      hex"6f0ee334fbba823804e14042c33bc5dfa5126e5076d8dcff02d4a045f266f427" ++
      hex"03" ++
      hex"93daaed620ff44fb7a760860ee8084f7f8c722d6c7ea9d1e1a35059253f876e6" ++
      hex"2c2faad9d5e25594914772dc815e157debf385cbe5de5a0aea59d15af42b19ad" ++
      hex"dd9cc1baf1453d682d27958c0f64c97a69249d655151c3b25b1ef1a993ec4f4f"
    val message = CompactFilterCheckPointMessage.fromBytes(bytes)

    assert(message.bytes == bytes)

    val biggerMessage = CompactFilterCheckPointMessage(
      filterType = FilterType.Basic,
      stopHash = DoubleSha256Digest.fromHex(
        "0000000000000000000000000000000000000000000000000000000000000001"),
      filterHeaders = 1.to(20000).toVector.map(_ => DoubleSha256Digest.empty)
    )

    val biggerBytes = biggerMessage.bytes
    assert(
      biggerBytes.size == 1 + // type size
        32 + // stop  hash size
        3 + // num filter headers size
        20000 * 32) // filter headers size

    val parsedBiggerMessage =
      CompactFilterCheckPointMessage.fromBytes(biggerBytes)

    assert(biggerMessage == parsedBiggerMessage)
    assert(biggerBytes == parsedBiggerMessage.bytes)
  }
}
