package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class NotFoundMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.notFoundMessage) { notFound =>
      assert(NotFoundMessage.fromBytes(notFound.bytes) == notFound)
    }
  }

  it must "parse a static test vector" in {
    val hex =
      "010200000054a02827d7a8b75601275a160279a3c5768de4c1c4a70200000000000000000000"
    val nfm = NotFoundMessage.fromHex(hex)
    assert(nfm.commandName == "notfound")
    assert(nfm.inventories.length == 1)
  }
}
