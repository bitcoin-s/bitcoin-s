package org.bitcoins.core.protocol

import org.bitcoins.core.config.{RegTest, TestNet3}
import org.bitcoins.core.gen.AddressGenerator
import org.scalacheck.{Prop, Properties}

class AddressSpec extends Properties("AddressSpec") {

  property("serialization symmetry") = {
    Prop.forAll(AddressGenerator.address) { addr =>
      val bool1 = Address
        .fromScriptPubKey(addr.scriptPubKey, addr.networkParameters)
        .get == addr
      val bool2 = if (addr.networkParameters == RegTest) {
        Address.fromString(addr.value).get == Address(addr.scriptPubKey,
                                                      TestNet3).get
      } else {
        Address.fromString(addr.value).get == addr
      }
      bool1 && bool2
    }
  }
}
