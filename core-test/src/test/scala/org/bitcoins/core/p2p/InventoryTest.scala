package org.bitcoins.core.p2p

import org.bitcoins.testkitcore.gen.p2p.DataMessageGenerator
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class InventoryTest extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(DataMessageGenerator.inventory) { inventory =>
      assert(Inventory(inventory.hex) == inventory)
    }
  }
}
