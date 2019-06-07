package org.bitcoins.core.p2p

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.testkit.core.gen.p2p.ControlMessageGenerator

class FeeFilterMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.feeFilterMessage) { fee =>
      assert(FeeFilterMessage.fromBytes(fee.bytes) == fee)
    }
  }
}
