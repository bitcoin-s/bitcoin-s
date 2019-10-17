package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.testkit.util.BitcoinSUnitTest

class RawServiceIdentifierSerializerTest extends BitcoinSUnitTest {

  "RawServiceIdentifierSerializer" must "read a unnamed service identfier from a hex string" in {
    val hex = "0000000000000000"
    assert(RawServiceIdentifierSerializer.read(hex).nodeNone)
  }
  it must "read a full node service identifier" in {
    val hex = "0100000000000000"
    assert(RawServiceIdentifierSerializer.read(hex).nodeNetwork)
  }

  it must "write a unnamed service identifier" in {
    val hex = "0000000000000000"
    val service = RawServiceIdentifierSerializer.read(hex)
    RawServiceIdentifierSerializer.write(service).toHex must be(hex)
  }

  it must "write a node network service provider" in {
    val hex = "0100000000000000"
    val service = RawServiceIdentifierSerializer.read(hex)
    RawServiceIdentifierSerializer.write(service).toHex must be(hex)
  }
}
