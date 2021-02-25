package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class NotFoundMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.notFoundMessage) { notFound =>
      assert(NotFoundMessage.fromBytes(notFound.bytes) == notFound)
    }
  }
}
