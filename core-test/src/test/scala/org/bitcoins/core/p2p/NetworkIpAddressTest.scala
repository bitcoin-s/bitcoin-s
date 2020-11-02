package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.P2PGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class NetworkIpAddressTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(P2PGenerator.networkIpAddress) { ip =>
      val fromBytes = NetworkIpAddress.fromBytes(ip.bytes)
      assert(fromBytes == ip)
    }
  }
}
