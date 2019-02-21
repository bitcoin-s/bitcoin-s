package org.bitcoins.node

import org.bitcoins.node.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 6/28/16.
  */
class NetworkMessageTest extends FlatSpec with MustMatchers {

  "NetworkMessage" must "be able to serialize then deserialize a message and get the original hex back" in {
    NetworkMessage(TestUtil.rawNetworkMessage).hex must be(
      TestUtil.rawNetworkMessage)
  }
}
