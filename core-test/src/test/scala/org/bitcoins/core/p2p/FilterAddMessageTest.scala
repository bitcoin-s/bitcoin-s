package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class FilterAddMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.filterAddMessage) { filterAddMsg =>
      assert(FilterAddMessage(filterAddMsg.hex) == filterAddMsg)
    }
  }
}
