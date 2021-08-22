package org.bitcoins.node

import org.bitcoins.testkit.rpc.BitcoindFixturesCachedPairV16

class PeerRelayTest extends BitcoindFixturesCachedPairV16 {

  behavior of "BitcoindV16RpcClient"

  it should "be able to get peer info" in { nodePair: FixtureParam =>
    val freshClient = nodePair.node1
    val otherFreshClient = nodePair.node2
    for {
      infoList <- freshClient.getPeerInfo
    } yield {
      assert(infoList.length >= 0)
      val info = infoList.head
      logger.info(infoList)
      assert(info.addnode)
      assert(info.networkInfo.addr == otherFreshClient.getDaemon.uri)
    }
  }
}
