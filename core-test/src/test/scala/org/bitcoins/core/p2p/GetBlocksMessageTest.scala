package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class GetBlocksMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getBlocksMessage) { getBlocks =>
      assert(getBlocks == GetBlocksMessage.fromBytes(getBlocks.bytes))
    }
  }
}
