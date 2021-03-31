package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class HeadersMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.headersMessage) { headersMsg =>
      assert(HeadersMessage(headersMsg.hex) == headersMsg)
    }
  }
}
