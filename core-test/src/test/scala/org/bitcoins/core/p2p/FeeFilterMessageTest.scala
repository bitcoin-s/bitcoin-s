package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.ControlMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class FeeFilterMessageTest extends BitcoinSUnitTest {
  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.feeFilterMessage) { fee =>
      assert(FeeFilterMessage.fromBytes(fee.bytes) == fee)
    }
  }
}
