package org.bitcoins.node.messages.data

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.testkit.gen.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class GetHeadersMessageSpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getHeaderMessages) { headerMsg =>
      assert(GetHeadersMessage(headerMsg.hex) == headerMsg)
    }
  }
}
