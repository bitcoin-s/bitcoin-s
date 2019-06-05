package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p.{NodeNetwork, UnnamedService}
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/2/16.
  */
class RawServiceIdentifierSerializerTest extends FlatSpec with MustMatchers {

  "RawServiceIdentifierSerializer" must "read a unnamed service identfier from a hex string" in {
    val hex = "0000000000000000"
    RawServiceIdentifierSerializer.read(hex) must be(UnnamedService)
  }
  it must "read a full node service identifier" in {
    val hex = "0100000000000000"
    RawServiceIdentifierSerializer.read(hex) must be(NodeNetwork)
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
