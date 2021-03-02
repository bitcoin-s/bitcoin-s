package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSJvmTest

class HeadersMessageTest extends BitcoinSJvmTest {

  it must "have serialization symmetry" in {
    forAllParallel(DataMessageGenerator.headersMessage) { headersMsg =>
      assert(HeadersMessage(headersMsg.hex) == headersMsg)
    }
  }
}
