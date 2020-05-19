package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, BitcoinSUnitTest}

import scala.concurrent.Future

class HeadersMessageTest extends BitcoinSAsyncTest {

  it must "have serialization symmetry" in {
    forAllAsync(DataMessageGenerator.headersMessage) { headersMsg =>
      Future {
        assert(HeadersMessage(headersMsg.hex) == headersMsg)
      }
    }
  }
}
