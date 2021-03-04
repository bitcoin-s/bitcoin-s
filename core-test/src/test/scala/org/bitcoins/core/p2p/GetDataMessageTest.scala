package org.bitcoins.core.p2p

import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class GetDataMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getDataMessages) { dataMsg =>
      assert(GetDataMessage(dataMsg.hex) == dataMsg)
    }
  }

  it must "be constructable from inventories" in {
    forAll(DataMessageGenerator.getDataMessages) { getData =>
      assert(GetDataMessage(getData.inventories) == getData)
    }
  }

  it must "be constructable from a single inventory" in {
    val inventory = Inventory(TypeIdentifier.MsgBlock, DoubleSha256Digest.empty)
    assert(GetDataMessage(inventory) == GetDataMessage(Seq(inventory)))
  }

  it must "have a meaningful toString" in {
    forAll(DataMessageGenerator.getDataMessages) { message =>
      assert(message.toString.length() < 200)
    }
  }
}
