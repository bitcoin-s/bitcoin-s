package org.bitcoins.rpc.clustermempool

import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.client.clustermempool.ClusterMempoolRpcClient
import org.bitcoins.testkit.rpc.BitcoindFixturesFundedCachedCluster

class ClusterMempoolRpcClientTest extends BitcoindFixturesFundedCachedCluster {

  behavior of "ClusterMempoolRpcClient"

  it must "get mempool info" in { case bitcoind: ClusterMempoolRpcClient =>
    for {
      _ <- bitcoind.generate(1)
      utxos <- bitcoind.listUnspent
      // make sure we have at least 2 spendable utxos
      // for the test case
      _ = assert(utxos.length > 1)
      info0 <- bitcoind.getMemPoolInfo
      addr0 <- bitcoind.getNewAddress
      _ <- bitcoind.sendToAddress(addr0, Bitcoins.one)
      info1 <- bitcoind.getMemPoolInfo
      addr1 <- bitcoind.getNewAddress
      _ <- bitcoind.sendToAddress(addr1, Bitcoins.one)
      info2 <- bitcoind.getMemPoolInfo

    } yield {
      assert(info0.numberofclusters == 0)
      assert(info0.maxclustercount == 0)
      assert(info0.maxclustersize == 0)
      assert(info0.size == 0)

      assert(info1.numberofclusters == 1)
      assert(info1.maxclustercount == 1)
      assert(info1.maxclustersize == 141)
      assert(info1.size == 1)

      assert(info2.size == 2)
      assert(info2.numberofclusters == 2)
      assert(info2.maxclustercount == 1)
    }
  }
}
