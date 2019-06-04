package org.bitcoins.node.messages.data

import org.bitcoins.testkit.gen.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class HeadersMessageSpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.headersMessage) { headersMsg =>
      assert(HeadersMessage(headersMsg.hex) == headersMsg)
    }
  }
}
