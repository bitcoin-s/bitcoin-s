package org.bitcoins.core.p2p

import org.bitcoins.testkit.gen.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class PongMessageSpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.pongMessage) { pongMsg =>
      assert(PongMessage(pongMsg.hex) == pongMsg)
    }
  }
}
