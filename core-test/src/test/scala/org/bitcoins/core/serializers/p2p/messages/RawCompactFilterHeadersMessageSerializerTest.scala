package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.p2p.CompactFilterHeadersMessage
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._

class RawCompactFilterHeadersMessageSerializerTest extends BitcoinSUnitTest {
  it must "parse a message" in {
    // cribbed from a P2P log dump with Bitcoin-S node
    val bytes =
      hex"00" ++ // type
        hex"226f1acd30ec19b541dba2478b673a142283bb39ccddef75015e3caf0ec89f48" ++ // stop hash
        hex"0000000000000000000000000000000000000000000000000000000000000000" ++ // previous filter hash
        hex"03" ++ // num filter hashes
        hex"1f30de30fabb7892d15eb985cc5d6c34c54a11b7e4c51f3da498f16255a27bb1" ++ // filter hashes
        hex"57965194aaa7ad3890c977d1b3c738d0a43a357ec645df28dc5c21876fb529c4" ++
        hex"87eb3f35daf3b6adba13b40c2f0d0e99dee59b624b0e09d870894e1a0d6d3bb0"
    val message = CompactFilterHeadersMessage.fromBytes(bytes)

    assert(message.filterType == FilterType.Basic)
    assert(
      message.stopHash == DoubleSha256Digest.fromHex(
        "226f1acd30ec19b541dba2478b673a142283bb39ccddef75015e3caf0ec89f48"))
    assert(message.previousFilterHeader == DoubleSha256Digest.empty)
    assert(
      message.filterHashes == Vector(
        DoubleSha256Digest.fromHex(
          "1f30de30fabb7892d15eb985cc5d6c34c54a11b7e4c51f3da498f16255a27bb1"),
        DoubleSha256Digest.fromHex(
          "57965194aaa7ad3890c977d1b3c738d0a43a357ec645df28dc5c21876fb529c4"),
        DoubleSha256Digest.fromHex(
          "87eb3f35daf3b6adba13b40c2f0d0e99dee59b624b0e09d870894e1a0d6d3bb0")
      ))
  }

  it must "have serialization symmetry" in {
    val bytes =
      hex"00" ++
        hex"226f1acd30ec19b541dba2478b673a142283bb39ccddef75015e3caf0ec89f48" ++
        hex"0000000000000000000000000000000000000000000000000000000000000000" ++
        hex"03" ++
        hex"1f30de30fabb7892d15eb985cc5d6c34c54a11b7e4c51f3da498f16255a27bb1" ++
        hex"57965194aaa7ad3890c977d1b3c738d0a43a357ec645df28dc5c21876fb529c4" ++
        hex"87eb3f35daf3b6adba13b40c2f0d0e99dee59b624b0e09d870894e1a0d6d3bb0"

    val message = CompactFilterHeadersMessage.fromBytes(bytes)

    assert(message.bytes == bytes)

    val biggerMessage = CompactFilterHeadersMessage(
      filterType = FilterType.Basic,
      stopHash = DoubleSha256Digest.fromHex(
        "0000000000000000000000000000000000000000000000000000000000000001"),
      previousFilterHeader = DoubleSha256Digest.fromHex(
        "0000000000000000000000000000000000000000000000000000000000000002"),
      filterHashes = 1.to(2000).toVector.map(_ => DoubleSha256Digest.empty)
    )

    val biggerBytes = biggerMessage.bytes
    assert(
      biggerBytes.size == 1 + // type size
        32 + // stop hash size
        32 + // previous filter header size
        3 + // num filters size
        2000 * 32) // filter hashes size

    val parsedBiggerMessage = CompactFilterHeadersMessage.fromBytes(biggerBytes)

    assert(biggerMessage == parsedBiggerMessage)
    assert(biggerBytes == parsedBiggerMessage.bytes)
  }
}
