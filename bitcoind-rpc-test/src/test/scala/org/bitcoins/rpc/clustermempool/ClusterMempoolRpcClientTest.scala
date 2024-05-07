package org.bitcoins.rpc.clustermempool

import org.bitcoins.rpc.client.clustermempool.ClusterMempoolRpcClient
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedCluster

class ClusterMempoolRpcClientTest extends BitcoindFixturesFundedCachedCluster {

  behavior of "ClusterMempoolRpcClient"

  it must "get mempool info" in { case bitcoind: ClusterMempoolRpcClient =>
    for {
      result <- bitcoind.getMemPoolInfo
    } yield {
      assert(result.numberofclusters == 0)
      assert(result.maxclustercount == 0)
      assert(result.maxclustersize == 0)
    }
  }
}
