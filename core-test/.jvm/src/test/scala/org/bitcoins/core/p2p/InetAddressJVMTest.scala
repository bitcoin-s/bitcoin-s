package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.P2PGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import java.net.{InetAddress => JvmAddress}

class InetAddressJVMTest extends BitcoinSUnitTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "InetAddress"

  it must "have serialization symmetry with java's InetAddress" in {
    forAll(P2PGenerator.inetAddress) { inet =>
      assert(
        NetworkIpAddress.writeAddress(
          JvmAddress.getByAddress(inet.getAddress).getAddress) == inet.bytes)
    }
  }

  it must "have serialization symmetry with java's InetAddress with IPv4" in {
    forAll(P2PGenerator.inetAddress) { inet =>
      assert(
        JvmAddress
          .getByAddress(inet.ipv4Bytes.toArray)
          .getAddress sameElements inet.ipv4Bytes.toArray)
    }
  }
}
