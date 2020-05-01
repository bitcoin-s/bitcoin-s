package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.{BitcoinSLogger, BytesUtil}
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 7/5/16.
  */
class RawHeadersMessageSerializerTest
    extends BitcoinSUnitTest
    with BitcoinSLogger {

  //from this example
  //https://bitcoin.org/en/developer-reference#headers
  val hex = "01" +
    "02000000" +
    "b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c0000000000000000" +
    "9d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab31471" +
    "24d95a54" +
    "30c31b18" +
    "fe9f0864" + "00"
  "RawHeadersMessageSerializer" must "deserialize a list of block headers" in {
    val headersMsg = RawHeadersMessageSerializer.read(hex)
    val header = headersMsg.headers.head
    headersMsg.count must be(CompactSizeUInt(UInt64.one, 1))
    header.previousBlockHash must be(
      DoubleSha256Digest(
        "b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c0000000000000000"))
    header.merkleRootHash must be(
      DoubleSha256Digest(
        "9d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab31471"))
    header.time must be(UInt32(1415239972))
    header.nBits must be(UInt32(BytesUtil.flipEndianness("30c31b18")))
    header.nonce must be(UInt32(BytesUtil.flipEndianness("fe9f0864")))
  }

  it must "read then write a HeaderMessage" in {
    val headersMsg = RawHeadersMessageSerializer.read(hex)
    RawHeadersMessageSerializer.write(headersMsg).toHex must be(hex)
  }

  it must "read the first two block headers from testnet3" in {
    val hex =
      "020100000043497fd7f826957108f4a30fd9cec3aeba79972084e90ead01ea330900000000bac8b0fa927c0ac8234287e33c5f74d38d354820e24756ad709d7038fc5f31f020e7494dffff001d03e4b672000100000006128e87be8b1b4dea47a7247d5528d2702c96826c7a648497e773b800000000e241352e3bec0a95a6217e10c3abb54adfa05abb12c126695595580fb92e222032e7494dffff001d00d2353400"
    val headersMsg = RawHeadersMessageSerializer.read(hex)
    val first = headersMsg.headers.head
    logger.debug("Headers: " + headersMsg.headers)
    first.previousBlockHash.hex must be(
      BytesUtil.flipEndianness(
        "000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943"))
    first.hash.hex must be(
      BytesUtil.flipEndianness(
        "00000000b873e79784647a6c82962c70d228557d24a747ea4d1b8bbe878e1206"))
    first.merkleRootHash.hex must be(
      BytesUtil.flipEndianness(
        "f0315ffc38709d70ad5647e22048358dd3745f3ce3874223c80a7c92fab0c8ba"))
    logger.debug("Second header: " + headersMsg.headers(1))
    val second = headersMsg.headers(1)
    second.previousBlockHash.hex must be(
      BytesUtil.flipEndianness(
        "00000000b873e79784647a6c82962c70d228557d24a747ea4d1b8bbe878e1206"))
    second.hash.hex must be(
      BytesUtil.flipEndianness(
        "000000006c02c8ea6e4ff69651f7fcde348fb9d557a06e6957b65552002a7820"))
    second.merkleRootHash.hex must be(
      BytesUtil.flipEndianness(
        "20222eb90f5895556926c112bb5aa0df4ab5abc3107e21a6950aec3b2e3541e2"))
  }
}
