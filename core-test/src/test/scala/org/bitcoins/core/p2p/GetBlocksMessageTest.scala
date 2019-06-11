package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class GetBlocksMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getBlocksMessage) { getBlocks =>
      assert(getBlocks == GetBlocksMessage.fromBytes(getBlocks.bytes))
    }
  }
}
