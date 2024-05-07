package org.bitcoins.rpc.clustermempool

import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedCluster

class ClusterMempoolRpcClientTest extends BitcoindFixturesFundedCachedCluster {

  behavior of "ClusterMempoolRpcClient"

  it must "get mempool info" in { case bitcoind =>
    for {
      _ <- bitcoind.getMemPoolInfo
    } yield {
      succeed
    }
  }
}
