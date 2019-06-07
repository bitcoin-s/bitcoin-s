package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class MerkleBlockMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.merkleBlockMessage) { merkleBlockMsg =>
      assert(MerkleBlockMessage(merkleBlockMsg.hex) == merkleBlockMsg)
    }
  }
}
