package org.bitcoins.core.p2p

import org.bitcoins.testkit.util.BitcoinSUnitTest

class ProtocolVersionTest extends BitcoinSUnitTest {

  "ProtocolVersion" must "give us the correct protocol version back from its hex format" in {
    ProtocolVersion("72110100") must be(ProtocolVersion70002)
  }
}
