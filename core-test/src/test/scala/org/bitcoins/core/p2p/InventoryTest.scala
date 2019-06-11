package org.bitcoins.core.p2p

import org.bitcoins.testkit.core.gen.p2p.DataMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class InventoryTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.inventory) { inventory =>
      assert(Inventory(inventory.hex) == inventory)
    }
  }
}
