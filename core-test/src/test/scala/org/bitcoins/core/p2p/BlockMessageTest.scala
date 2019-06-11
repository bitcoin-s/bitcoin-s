package org.bitcoins.core.p2p

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator

class BlockMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.blockMessage) { block =>
      assert(block == BlockMessage.fromBytes(block.bytes))
    }
  }
}
