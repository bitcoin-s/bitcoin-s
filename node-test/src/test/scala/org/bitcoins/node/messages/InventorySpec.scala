package org.bitcoins.node.messages

import org.bitcoins.testkit.gen.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class InventorySpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.inventory) { inventory =>
      assert(Inventory(inventory.hex) == inventory)
    }
  }
}
