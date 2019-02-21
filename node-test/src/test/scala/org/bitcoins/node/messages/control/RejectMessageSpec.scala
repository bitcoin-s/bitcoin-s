package org.bitcoins.node.messages.control

import org.bitcoins.node.gen.ControlMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 8/31/16.
  */
class RejectMessageSpec extends Properties("RejectMessageSpec") {

  property("serialization symmetry") = {
    Prop.forAll(ControlMessageGenerator.rejectMessage) {
      case rejectMsg =>
        RejectMessage(rejectMsg.hex) == rejectMsg

    }
  }
}
