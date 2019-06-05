package org.bitcoins.core.p2p

import org.bitcoins.testkit.gen.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class PingMessageSpec extends BitcoinSUnitTest {

  it must "have symmetry serialization" in {
    forAll(ControlMessageGenerator.pingMessage) { pingMessage =>
      assert(PingMessage(pingMessage.hex) == pingMessage)
    }
  }
}
