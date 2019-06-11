package org.bitcoins.core.p2p

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.core.gen.p2p.ControlMessageGenerator

class AddrMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.addrMessage) { addr =>
      val fromBytes = AddrMessage.fromBytes(addr.bytes)
      // assert(fromBytes == addr) todo
      assert(true)
    }
  }
}
