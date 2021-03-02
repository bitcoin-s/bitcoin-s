package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.P2PGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class NetworkIpAddressTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(P2PGenerator.networkIpAddress) { ip =>
      val fromBytes = NetworkIpAddress.fromBytes(ip.bytes)
      assert(fromBytes == ip)
    }
  }
}
