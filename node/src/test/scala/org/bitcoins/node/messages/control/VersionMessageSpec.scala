package org.bitcoins.node.messages.control

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.gen.ControlMessageGenerator
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 6/27/16.
  */
class VersionMessageSpec
    extends Properties("VersionMessageSpec")
    with BitcoinSLogger {

  property("Serialization symmetry") =
    Prop.forAll(ControlMessageGenerator.versionMessage) { versionMessage =>
      VersionMessage(versionMessage.hex) == versionMessage

    }

}
