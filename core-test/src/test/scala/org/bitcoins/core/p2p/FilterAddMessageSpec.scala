package org.bitcoins.core.p2p

import org.bitcoins.testkit.gen.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 8/26/16.
  */
class FilterAddMessageSpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.filterAddMessage) { filterAddMsg =>
      assert(FilterAddMessage(filterAddMsg.hex) == filterAddMsg)
    }
  }
}
