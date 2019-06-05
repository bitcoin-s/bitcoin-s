package org.bitcoins.core.p2p

import org.bitcoins.testkit.gen.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class FilterLoadMessageSpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.filterLoadMessage) { filterMsg =>
      assert(FilterLoadMessage(filterMsg.hex) == filterMsg)
    }
  }
}
