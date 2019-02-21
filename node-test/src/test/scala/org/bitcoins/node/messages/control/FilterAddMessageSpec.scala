package org.bitcoins.node.messages.control

import org.bitcoins.node.gen.ControlMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 8/26/16.
  */
class FilterAddMessageSpec extends Properties("FilterAddMessageSpec") {

  property("Serialization symmetry") =
    Prop.forAll(ControlMessageGenerator.filterAddMessage) {
      case filterAddMsg =>
        FilterAddMessage(filterAddMsg.hex) == filterAddMsg
    }
}
