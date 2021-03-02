package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class RejectMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.rejectMessage) { rejectMsg =>
      assert(RejectMessage(rejectMsg.hex) == rejectMsg)
    }
  }
}
