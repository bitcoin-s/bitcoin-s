package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class RejectMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.rejectMessage) { rejectMsg =>
      assert(RejectMessage(rejectMsg.hex) == rejectMsg)
    }
  }
}
