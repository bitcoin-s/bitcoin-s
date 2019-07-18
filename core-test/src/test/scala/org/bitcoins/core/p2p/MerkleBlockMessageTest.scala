package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits._
import org.bitcoins.node.util.BitcoinSpvNodeUtil
import org.bitcoins.core.crypto.DoubleSha256Digest

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

  // we had a bug where we didn't consume the right number of bytes
  // when parsing a merkle block message, thereby screwing up
  // the parsing of the remainder
  it must "parse a byte vector with three messages in it" in {
    val bytes =
      hex"fabfb5da6d65726b6c65626c6f636b0097000000b4b6e45d00000020387191f7d488b849b4080fdf105c71269fc841a2f0f2944fc5dc785c830c716e37f36373098aae06a668cc74e388caf50ecdcb5504ce936490b4b72940e08859548c305dffff7f20010000000200000002ecd1c722709bfc241f8b94fc64034dcba2c95409dc4cd1d7b864e1128a04e5b044133327b04ff8ac576e7748a4dae4111f0c765dacbfe0c5a9fddbeb8f60d5af0105fabfb5da747800000000000000000000cc0100004413332702000000065b7f0f3eec398047e921037815aa41709b6243a1897f1423194b7558399ae0300000000017160014008dc9d88d1797305f3fbd30d2b36d6bde984a09feffffffe9145055d671fd705a09f028033da614b619205b9926fe5ebe45e15ae8b3231e0100000017160014d74cfac04bb0e6838c35f1f4a0a60d13655be2fbfeffffff797f8ff9c10fa618b6254343a648be995410e82c03fd8accb0de2271a3fb1abd00000000171600143ee832c09db48eca28a64a358ed7a01dbe52d31bfeffffffc794dba971b9479dfcbc662a3aacd641553bdb2418b15c0221c5dfd4471a7a70000000001716001452c13ba0314f7718c234ed6adfea6422ce03a545feffffffb7c3bf1762b15f3b0e0eaa5beb46fe96a9e2829a7413fd900b9b7e0d192ab64800000000171600143ee832c09db48eca28a64a358ed7a01dbe52d31bfeffffffb6ced6cb8dfc2f7f5b37561938ead3bc5ca4036e2b45d9738cc086a10eed4e010100000017160014aebb17e245fe8c98a75f0b6717fcadca30e491e2feffffff02002a7515000000001976a9148374ff8beb55ea2945039881ca26071b5749fafe88ac485620000000000017a91405d36a2b0bdedf3fc58bed6f9e4026f8934a2716876b050000fabfb5da686561646572730000000000010000001406e05800"
    val (messages, leftover) = BitcoinSpvNodeUtil.parseIndividualMessages(bytes)
    assert(messages.length == 3)
    assert(leftover.isEmpty)

  }
}
