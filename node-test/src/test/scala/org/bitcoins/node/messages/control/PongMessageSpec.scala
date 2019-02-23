package org.bitcoins.node.messages.control

import org.bitcoins.testkit.gen.ControlMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 7/5/16.
  */
class PongMessageSpec extends Properties("PongMessageSpec") {

  property("Serialization symmetry") =
    Prop.forAll(ControlMessageGenerator.pongMessage) { pongMsg =>
      PongMessage(pongMsg.hex) == pongMsg
    }
}
