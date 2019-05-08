package org.bitcoins.node

import org.bitcoins.node.util.NodeTestUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 6/28/16.
  */
class NetworkMessageTest extends BitcoinSUnitTest {

  "NetworkMessage" must "be able to serialize then deserialize a message and get the original hex back" in {
    NetworkMessage(NodeTestUtil.rawNetworkMessage).hex must be(
      NodeTestUtil.rawNetworkMessage)
  }
}
