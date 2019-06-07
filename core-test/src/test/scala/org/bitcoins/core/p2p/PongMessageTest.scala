package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class PongMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.pongMessage) { pongMsg =>
      assert(PongMessage(pongMsg.hex) == pongMsg)
    }
  }
}
