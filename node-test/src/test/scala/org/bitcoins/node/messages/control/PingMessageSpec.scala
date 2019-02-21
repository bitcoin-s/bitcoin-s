package org.bitcoins.node.messages.control

import org.bitcoins.node.gen.ControlMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/29/16.
  */
class PingMessageSpec extends Properties("PingMessageSpec") {

  property("Symmetry serialization") =
    Prop.forAll(ControlMessageGenerator.pingMessage) { pingMessage =>
      PingMessage(pingMessage.hex) == pingMessage
    }
}
