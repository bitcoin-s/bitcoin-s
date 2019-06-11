package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class FilterAddMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.filterAddMessage) { filterAddMsg =>
      assert(FilterAddMessage(filterAddMsg.hex) == filterAddMsg)
    }
  }
}
