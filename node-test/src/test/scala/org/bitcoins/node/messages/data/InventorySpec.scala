package org.bitcoins.node.messages.data

import org.bitcoins.testkit.gen.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 7/8/16.
  */
class InventorySpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.inventory) { inventory =>
      assert(Inventory(inventory.hex) == inventory)
    }
  }
}
