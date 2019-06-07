package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen

class GetDataMessageTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.getDataMessages) { dataMsg =>
      assert(GetDataMessage(dataMsg.hex) == dataMsg)
    }
  }

  it must "be constructable from inventories" in {
    forAll(DataMessageGenerator.getDataMessages) { getData =>
      getData.inventories match {
        case head :: Nil => assert(GetDataMessage(head) == getData)
        case other =>
          assert(GetDataMessage(other) == getData)
      }
    }
  }
}
