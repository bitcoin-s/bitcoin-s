package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class PingMessageTest extends BitcoinSUnitTest {

  it must "have symmetry serialization" in {
    forAll(ControlMessageGenerator.pingMessage) { pingMessage =>
      assert(PingMessage(pingMessage.hex) == pingMessage)
    }
  }
}
