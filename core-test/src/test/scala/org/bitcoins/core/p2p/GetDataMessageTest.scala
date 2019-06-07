package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class GetDataMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getDataMessages) { dataMsg =>
      assert(GetDataMessage(dataMsg.hex) == dataMsg)
    }
  }
}
