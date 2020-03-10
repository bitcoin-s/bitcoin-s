---
id: chain-query-api
title: chain-query-api
---

```scala mdoc:invisible
import akka.actor.ActorSystem
import org.bitcoins.core.api.ChainQueryApi
import org.bitcoins.core.api.ChainQueryApi.FilterResponse
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.gcs.{FilterType, GolombFilter}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.Transaction
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

### ChainQueryApi

The ChainQueryApi is how the wallet project stays aware of the current best chain.
This allows the wallet for example to calculate the number of confirmations for a transaction,
get the current chain tip, or even retrieve block filters for a given set of blocks.

Since this is an API it can be hooked up to the `chain` module of bitcoin-s but it can also be linked to
any other implementation of your choosing. This allows you to use the bitcoin-s wallet in any schema that you
want.

The functions that the ChainQueryApi supports are:

```scala mdoc:compile-only
trait ChainQueryApi {

  /** Gets the height of the given block */
  def getBlockHeight(blockHash: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the hash of the block that is what we consider "best" */
  def getBestBlockHash(): Future[DoubleSha256DigestBE]

  /** Gets number of confirmations for the given block hash*/
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the number of compact filters in the database */
  def getFilterCount: Future[Int]

  /** Returns the block height of the given block stamp */
  def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int]

  def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]]
}
```

## Chain query with bitcoind

As an example, we will show you how to use the `ChainQueryApi` and bitcoind to query chain data.

```scala mdoc:compile-only
implicit val system: ActorSystem = ActorSystem(s"node-api-example")
implicit val ec: ExecutionContextExecutor = system.dispatcher
implicit val walletConf: WalletAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig().walletConf

// let's use a helper method to get a v19 bitcoind
// and a ChainApi
val bitcoind = BitcoindV19RpcClient(BitcoindInstance.fromConfigFile())
val nodeApi = BitcoinSWalletTest.MockNodeApi

// Create our key manager
val keyManagerE = BIP39KeyManager.initialize(kmParams = walletConf.kmParams,
                                               bip39PasswordOpt = None)

val keyManager = keyManagerE match {
    case Right(keyManager) => keyManager
    case Left(err) =>
      throw new RuntimeException(s"Cannot initialize key manager err=$err")
  }

// This function can be used to create a callback for when our chain api receives a transaction, block, or
// a block filter, the returned NodeCallbacks will contain the necessary items to initialize the callbacks
def createCallbacks(
      processTransaction: Transaction => Unit,
      processCompactFilter: (DoubleSha256Digest, GolombFilter) => Unit,
      processBlock: Block => Unit): NodeCallbacks = {
    lazy val onTx: OnTxReceived = { tx =>
      processTransaction(tx)
      ()
    }
    lazy val onCompactFilter: OnCompactFilterReceived = {
      (blockHash, blockFilter) =>
        processCompactFilter(blockHash, blockFilter)
        ()
    }
    lazy val onBlock: OnBlockReceived = { block =>
      processBlock(block)
    }
    NodeCallbacks(onTxReceived = Seq(onTx),
                  onBlockReceived = Seq(onBlock),
                  onCompactFilterReceived = Seq(onCompactFilter))
  }

// Here is a super simple example of a callback, this could be replaced with anything, from
// relaying the block on the network, finding relevant wallet transactions, verifying the block,
// or writing it to disk
val exampleProcessTx = (tx: Transaction) =>
    println(s"Received tx: ${tx.txIdBE}")

val exampleProcessBlock = (block: Block) =>
    println(s"Received block: ${block.blockHeader.hashBE}")

val exampleProcessFilter =
    (blockHash: DoubleSha256Digest, filter: GolombFilter) =>
      println(s"Received filter: ${blockHash.flip.hex} ${filter.hash.flip.hex}")

val exampleCallbacks =
    createCallbacks(exampleProcessTx, exampleProcessFilter, exampleProcessBlock)

// Here is where we are defining our actual chain api, Ideally this could be it's own class
// but for the examples sake we will keep it small.
val chainApi = new ChainQueryApi {

    /** Gets the height of the given block */
    override def getBlockHeight(
        blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
      bitcoind.getBlock(blockHash).map(block => Some(block.height))
    }

    /** Gets the hash of the block that is what we consider "best" */
    override def getBestBlockHash(): Future[DoubleSha256DigestBE] = {
      bitcoind.getBestBlockHash
    }

    /** Gets number of confirmations for the given block hash */
    override def getNumberOfConfirmations(
        blockHash: DoubleSha256DigestBE): Future[Option[Int]] = {
      for {
        tip <- bitcoind.getBlockCount
        block <- bitcoind.getBlock(blockHash)
      } yield {
        Some(tip - block.height + 1)
      }
    }

    /** Gets the number of compact filters in the database */
    override def getFilterCount: Future[Int] = {
      // since bitcoind should have the filter for
      // every block we can just return the block height
      bitcoind.getBlockCount
    }

    /** Returns the block height of the given block stamp */
    override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] = {
      blockStamp match {
        case blockHeight: BlockStamp.BlockHeight =>
          Future.successful(blockHeight.height)
        case blockHash: BlockStamp.BlockHash =>
          getBlockHeight(blockHash.hash).map(_.get)
        case blockTime: BlockStamp.BlockTime =>
          Future.failed(new RuntimeException(s"Not implemented: $blockTime"))
      }
    }

    override def getFiltersBetweenHeights(
        startHeight: Int,
        endHeight: Int): Future[Vector[FilterResponse]] = {
      val filterFs = startHeight
        .until(endHeight)
        .map { height =>
          for {
            hash <- bitcoind.getBlockHash(height)
            filter <- bitcoind.getBlockFilter(hash, FilterType.Basic)
          } yield {
            FilterResponse(filter.filter, hash, height)
          }
        }
        .toVector

      Future.sequence(filterFs)
    }
  }

// Finally, we can initialize our wallet with our own node api
val wallet =
    Wallet(keyManager = keyManager, nodeApi = nodeApi, chainQueryApi = chainApi)

// Then to trigger one of the events we can run
wallet.chainQueryApi.getFiltersBetweenHeights(100, 150)

```