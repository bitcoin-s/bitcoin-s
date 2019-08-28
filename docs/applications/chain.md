---
title: Blockchain Verification
id: chain
---

Bitcoin-S comes bundled with a rudimentary blockchain verification
module. This module is currently only released as a library, and not as a binary.
This is because it (nor the documentation) is not deemed production
ready. Use at your own risk, and without too much money depending on it.

## Syncing and verifying block headers

Using the `chain` module of Bitcoin-S it's possible to
sync and verify block headers from the Bitcoin blockchain. In this document
we demonstrate how to do this, while persisting it to disk. We should be
able to read this chain on subsequent runs, assuming we are connected
to the same `bitcoind` instance.

```scala mdoc:compile-only
import org.bitcoins.chain.blockchain._
import org.bitcoins.chain.blockchain.sync._
import org.bitcoins.chain.models._

import org.bitcoins.rpc.client.common._
import org.bitcoins.testkit.chain._

import scala.concurrent._

implicit val ec = ExecutionContext.global

// We are assuming that a `bitcoind` regtest node is running the background.
// You can see our `bitcoind` guides to see how to connect
// to a local or remote `bitcoind` node.

import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.client.common.BitcoindRpcClient

val bitcoindInstance = BitcoindInstance.fromDatadir()
val rpcCli = BitcoindRpcClient(bitcoindInstance)

// Next, we need to create a way to monitor the chain:

val getBestBlockHash = ChainTestUtil.bestBlockHashFnRpc(Future.successful(rpcCli))

val getBlockHeader = ChainTestUtil.getBlockHeaderFnRpc(Future.successful(rpcCli))


// set a data directory
import java.nio.file.Files
val datadir = Files.createTempDirectory("bitcoin-s-test")

// set the currenet network to regtest
import com.typesafe.config.ConfigFactory
val config = ConfigFactory.parseString {
    """
    | bitcoin-s {
    |   network = regtest
    | }
    |""".stripMargin
}

import org.bitcoins.chain.config.ChainAppConfig
implicit val chainConfig = ChainAppConfig(datadir, config)

// Initialize the needed database tables if they don't exist:
val chainProjectInitF = chainConfig.initialize()
val blockHeaderDAO = BlockHeaderDAO()

// Now, do the actual syncing:
val chainHandlerF = ChainHandler.fromDatabase(blockHeaderDAO)

val syncedChainApiF = for {
    _ <- chainProjectInitF
    handler <- chainHandlerF
    synced <- ChainSync.sync(handler, getBlockHeader, getBestBlockHash)
} yield synced


val syncResultF = syncedChainApiF.flatMap { chainApi =>
  chainApi.getBlockCount.map(count => println(s"chain api blockcount=${count}"))

  rpcCli.getBlockCount.map(count => println(s"bitcoind blockcount=${count}"))
}

syncResultF.onComplete { case result =>
  println(s"Sync result=${result}")
}
```
