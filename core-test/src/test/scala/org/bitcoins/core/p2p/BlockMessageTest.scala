package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSAsyncTest

class BlockMessageTest extends BitcoinSAsyncTest {
  it must "have serialization symmetry" in {
    forAllParallel(DataMessageGenerator.blockMessage) { block =>
      assert(block == BlockMessage.fromBytes(block.bytes))
    }
  }
}
