package org.bitcoins.node.messages.data

import org.bitcoins.node.gen.DataMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 7/8/16.
  */
class InventorySpec extends Properties("InventorySpec") {

  property("Serialization symmetry") =
    Prop.forAll(DataMessageGenerator.inventory) { inventory =>
      Inventory(inventory.hex) == inventory

    }
}
