package org.bitcoins.core.p2p

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator

class NotFoundMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.notFoundMessage) { notFound =>
      assert(NotFoundMessage.fromBytes(notFound.bytes) == notFound)
    }
  }
}
