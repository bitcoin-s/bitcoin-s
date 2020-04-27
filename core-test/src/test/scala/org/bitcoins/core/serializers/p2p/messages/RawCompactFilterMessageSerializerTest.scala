package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.gcs.{BlockFilter, FilterType, GolombFilter}
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class RawCompactFilterMessageSerializerTest extends BitcoinSUnitTest {
  it must "parse a message" in {
    // cribbed from a P2P log dump with Bitcoin-S node
    val bytes =
      hex"00" ++ // type
        hex"6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000" ++ // block hash
        hex"04" ++ // num bytes
        hex"017fa880" // filter bytes

    val message = CompactFilterMessage.fromBytes(bytes)

    assert(message.filterType == FilterType.Basic)
    assert(
      message.blockHash == DoubleSha256Digest.fromHex(
        "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"))
    assert(message.filterBytes == hex"017fa880")
  }

  it must "have serialization symmetry" in {
    val bytes =
      hex"00" ++
        hex"6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000" ++
        hex"04" ++
        hex"017fa880"

    val message = CompactFilterMessage.fromBytes(bytes)

    assert(message.bytes == bytes)

    val blockHash = DoubleSha256Digest.fromHex(
      "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000")
    val fromFilter =
      CompactFilterMessage(blockHash,
                           BlockFilter.fromBytes(hex"017fa880", blockHash))

    assert(message.bytes == fromFilter.bytes)

    val biggerMessage = CompactFilterMessage(
      filterType = FilterType.Basic,
      blockHash = DoubleSha256Digest.fromHex(
        "0000000000000000000000000000000000000000000000000000000000000001"),
      filterBytes = ByteVector.fill(100000)(0xaa)
    )

    val biggerBytes = biggerMessage.bytes
    assert(
      biggerBytes.size == 1 + // type size
        32 + // block hash size
        5 + // num bytes size
        100000) // filter bytes size

    val parsedBiggerMessage = CompactFilterMessage.fromBytes(biggerBytes)

    assert(biggerMessage == parsedBiggerMessage)
    assert(biggerBytes == parsedBiggerMessage.bytes)
  }
}
