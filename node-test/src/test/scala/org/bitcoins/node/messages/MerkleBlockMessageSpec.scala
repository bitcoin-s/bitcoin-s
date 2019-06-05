package org.bitcoins.node.messages

import org.bitcoins.testkit.gen.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class MerkleBlockMessageSpec extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.merkleBlockMessage) { merkleBlockMsg =>
      assert(MerkleBlockMessage(merkleBlockMsg.hex) == merkleBlockMsg)
    }
  }
}
