package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class PongMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.pongMessage) { pongMsg =>
      assert(PongMessage(pongMsg.hex) == pongMsg)
    }
  }
}
