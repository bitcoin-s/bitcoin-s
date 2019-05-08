package org.bitcoins.node.messages.control

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.testkit.gen.ControlMessageGenerator
import org.bitcoins.testkit.util.BitcoinSUnitTest

class VersionMessageSpec extends BitcoinSUnitTest {

  it must "have serialization symmetry" in {
    forAll(ControlMessageGenerator.versionMessage) { versionMessage =>
      assert(VersionMessage(versionMessage.hex) == versionMessage)
    }
  }
}
