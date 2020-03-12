---
id: node-api
title: Node API
---

```scala mdoc:invisible
import akka.actor.ActorSystem
import org.bitcoins.core.api.NodeApi
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.node.NodeCallbacks
import org.bitcoins.node.networking.peer.DataMessageHandler._
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContextExecutor, Future}
```

### NodeAPI

The NodeApi is how the wallet project retrieves relevant node data like blocks.
This allows the wallet for example to retrieve blocks for finding its relevant transactions.

Since this is an API it can be hooked up to the `node` module of bitcoin-s but it can also be linked to
any other implementation of your choosing. This allows you to use the bitcoin-s wallet in any schema that you
want.

The functions that the NodeApi supports are:

```scala mdoc:compile-only
trait NodeApi {

  /** Request the underlying node to download the given blocks from its peers and feed the blocks to [[org.bitcoins.node.NodeCallbacks]] */
    def downloadBlocks(blockHashes: Vector[DoubleSha256Digest]): Future[Unit]
}
```

## Downloading blocks with bitcoind

As an example, we will show you how to use the `NodeApi` and bitcoind to download blocks for a wallet.

```scala mdoc:compile-only
implicit val system: ActorSystem = ActorSystem(s"node-api-example")
implicit val ec: ExecutionContextExecutor = system.dispatcher
implicit val walletConf: WalletAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig().walletConf

// let's use a helper method to get a v19 bitcoind
// and a ChainApi
val bitcoind = BitcoindV19RpcClient(BitcoindInstance.fromConfigFile())
val chainApi = BitcoinSWalletTest.MockChainQueryApi

// Create our key manager
val keyManagerE = BIP39KeyManager.initialize(kmParams = walletConf.kmParams,
                                               bip39PasswordOpt = None)

val keyManager = keyManagerE match {
    case Right(keyManager) => keyManager
    case Left(err) =>
      throw new RuntimeException(s"Cannot initialize key manager err=$err")
  }

// This function can be used to create a callback for when our node api calls downloadBlocks,
// more specifically it will call the function every time we receive a block, the returned
// NodeCallbacks will contain the necessary items to initialize the callbacks
def createCallback(processBlock: Block => Unit): NodeCallbacks = {
    lazy val onBlock: OnBlockReceived = { block =>
      processBlock(block)
    }
    NodeCallbacks(onBlockReceived = Seq(onBlock))
  }

// Here is a super simple example of a callback, this could be replaced with anything, from
// relaying the block on the network, finding relevant wallet transactions, verifying the block,
// or writing it to disk
val exampleProcessBlock = (block: Block) =>
    println(s"Received block: ${block.blockHeader.hashBE}")
val exampleCallback = createCallback(exampleProcessBlock)

// Here is where we are defining our actual node api, Ideally this could be it's own class
// but for the examples sake we will keep it small.
  val nodeApi = new NodeApi {
    override def downloadBlocks(
        blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
      val blockFs = blockHashes.map(hash => bitcoind.getBlockRaw(hash))
      Future.sequence(blockFs).map(_ => ())
    }
  }

// Finally, we can initialize our wallet with our own node api
val wallet =
    Wallet(keyManager = keyManager, nodeApi = nodeApi, chainQueryApi = chainApi)

// Then to trigger the event we can run
val exampleBlock = DoubleSha256Digest(
    "000000000010dc23dc0d5acad64667a7a2b3010b6e02da4868bf392c90b6431d")
wallet.nodeApi.downloadBlocks(Vector(exampleBlock))

```