package org.bitcoins.core.p2p

import org.bitcoins.testkit.node.NodeTestUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

class NetworkMessageTest extends BitcoinSUnitTest {

  "NetworkMessage" must "be able to serialize then deserialize a message and get the original hex back" in {
    NetworkMessage(NodeTestUtil.rawNetworkMessage).hex must be(
      NodeTestUtil.rawNetworkMessage)
  }
}
