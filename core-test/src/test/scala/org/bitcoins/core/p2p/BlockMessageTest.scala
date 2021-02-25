package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

class BlockMessageTest extends BitcoinSJvmTest {
  it must "have serialization symmetry" in {
    forAllParallel(DataMessageGenerator.blockMessage) { block =>
      assert(block == BlockMessage.fromBytes(block.bytes))
    }
  }
}
