package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class PingMessageTest extends BitcoinSUnitTest {

  it must "have symmetry serialization" in {
    forAll(ControlMessageGenerator.pingMessage) { pingMessage =>
      assert(PingMessage(pingMessage.hex) == pingMessage)
    }
  }
}
