package org.bitcoins.node.messages

import org.bitcoins.testkit.gen.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class InventoryMessageSpec extends BitcoinSUnitTest {

  it must " have serialization symmetry" in {
    forAll(DataMessageGenerator.inventoryMessages) { invMessage =>
      assert(InventoryMessage(invMessage.hex) == invMessage)
    }
  }
}
