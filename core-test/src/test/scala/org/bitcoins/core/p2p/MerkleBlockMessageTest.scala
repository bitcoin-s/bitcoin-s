package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.node.networking.P2PClient

class MerkleBlockMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.merkleBlockMessage) { merkleBlockMsg =>
      assert(MerkleBlockMessage(merkleBlockMsg.hex) == merkleBlockMsg)
    }
  }

  // https://bitcoin.org/en/developer-reference#merkleblock
  it must "parse the example from Bitcoin.org" in {
    val bytes =
      hex"0100000082bb869cf3a793432a66e826e05a6fc37469f8efb7421dc880670100000000007f16c5962e8bd963659c793ce370d95f093bc7e367117b3c30c1f8fdd0d9728776381b4d4c86041b554b852907000000043612262624047ee87660be1a707519a443b1c1ce3d248cbfc6c15870f6c5daa2019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b6541ed70551dd7e841883ab8f0b16bf04176b7d1480e4f0af9f3d4c3595768d06820d2a7bc994987302e5b1ac80fc425fe25f8b63169ea78e68fbaaefa59379bbf011d"
    val message = MerkleBlockMessage.fromBytes(bytes)
    val merkle = message.merkleBlock

    assert(merkle.transactionCount.toInt == 7)
    assert(merkle.hashCount.toInt == 4)

    assert(merkle.hashes.length == 4)

    val Seq(first, second, third, fourth) = merkle.hashes
    val expectedFirst =
      DoubleSha256Digest.fromHex(
        "3612262624047ee87660be1a707519a443b1c1ce3d248cbfc6c15870f6c5daa2")
    val expectedSecond = DoubleSha256Digest.fromHex(
      "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65")
    val expectedThird = DoubleSha256Digest.fromHex(
      "41ed70551dd7e841883ab8f0b16bf04176b7d1480e4f0af9f3d4c3595768d068")
    val expectedFourth = DoubleSha256Digest.fromHex(
      "20d2a7bc994987302e5b1ac80fc425fe25f8b63169ea78e68fbaaefa59379bbf")

    assert(first == expectedFirst)
    assert(second == expectedSecond)
    assert(third == expectedThird)
    assert(fourth == expectedFourth)
  }

}
