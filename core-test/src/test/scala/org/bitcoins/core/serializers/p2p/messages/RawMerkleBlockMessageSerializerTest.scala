package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.BitVector

class RawMerkleBlockMessageSerializerTest extends BitcoinSUnitTest {

  //from bitcoin developer reference
  //https://bitcoin.org/en/developer-reference#merkleblock
  val hex =
    "0100000082bb869cf3a793432a66e826e05a6fc37469f8efb7421dc88067010000000000" +
      "7f16c5962e8bd963659c793ce370d95f093bc7e367117b3c30c1f8fdd0d97287" +
      "76381b4d4c86041b554b85290700000004" +
      "3612262624047ee87660be1a707519a443b1c1ce3d248cbfc6c15870f6c5daa2" +
      "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65" +
      "41ed70551dd7e841883ab8f0b16bf04176b7d1480e4f0af9f3d4c3595768d068" +
      "20d2a7bc994987302e5b1ac80fc425fe25f8b63169ea78e68fbaaefa59379bbf" +
      "011d"

  "RawMerkleBlockMessage" must "read a raw hex string into a merkle block message" in {
    val merkleBlockMessage = RawMerkleBlockMessageSerializer.read(hex)

    merkleBlockMessage.merkleBlock.transactionCount must be(UInt32(7))
    merkleBlockMessage.merkleBlock.hashCount must be(CompactSizeUInt(UInt64(4)))

    merkleBlockMessage.merkleBlock.hashes must be(
      Seq(
        DoubleSha256Digest(BytesUtil.decodeHex(
          "3612262624047ee87660be1a707519a443b1c1ce3d248cbfc6c15870f6c5daa2")),
        DoubleSha256Digest(BytesUtil.decodeHex(
          "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65")),
        DoubleSha256Digest(BytesUtil.decodeHex(
          "41ed70551dd7e841883ab8f0b16bf04176b7d1480e4f0af9f3d4c3595768d068")),
        DoubleSha256Digest(BytesUtil.decodeHex(
          "20d2a7bc994987302e5b1ac80fc425fe25f8b63169ea78e68fbaaefa59379bbf"))
      ))

    merkleBlockMessage.merkleBlock.partialMerkleTree.bits must be(
      BitVector.fromValidBin("10111000"))
  }

  it must "write a merkle block header message" in {
    val merkleBlockMessage = RawMerkleBlockMessageSerializer.read(hex)

    RawMerkleBlockMessageSerializer.write(merkleBlockMessage).toHex must be(hex)
  }
}
