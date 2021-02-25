package org.bitcoins.core.serializers.p2p

import org.bitcoins.core.number.UInt32
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class RawNetworkIpAddressSerializerTest extends BitcoinSUnitTest {

  //from this bitcoin developer guide example
  //https://bitcoin.org/en/developer-reference#addr
  val time = "d91f4854"
  val services = "0100000000000000"
  val address = "00000000000000000000ffffc0000233"
  val port = "208d"
  val hex = time + services + address + port
  "RawNetworkIpAddressSerializer" must "read a network ip address from a hex string" in {
    val ipAddress = RawNetworkIpAddressSerializer.read(hex)
    ipAddress.time must be(UInt32(1414012889))
    assert(ipAddress.services.nodeNetwork)
    ipAddress.address.toString must be("/192.0.2.51")
    ipAddress.port must be(8333)
  }

  it must "write a network ip address from and get its original hex back" in {
    val ipAddress = RawNetworkIpAddressSerializer.read(hex)
    RawNetworkIpAddressSerializer.write(ipAddress).toHex must be(hex)
  }
}
