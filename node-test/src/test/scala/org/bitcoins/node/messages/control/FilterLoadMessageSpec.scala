package org.bitcoins.node.messages.control

import org.bitcoins.testkit.gen.ControlMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 7/20/16.
  */
class FilterLoadMessageSpec extends Properties("FilterLoadMessageSpec") {

  property("Serialization symmetry") =
    Prop.forAll(ControlMessageGenerator.filterLoadMessage) { filterMsg =>
      FilterLoadMessage(filterMsg.hex) == filterMsg
    }
}
