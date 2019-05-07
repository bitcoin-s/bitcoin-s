import org.bitcoins.rpc.config._

import akka.actor.ActorSystem
import org.bitcoins.chain.db._
import org.bitcoins.chain.config._
import org.bitcoins.chain.blockchain._
import org.bitcoins.chain.blockchain.sync._
import org.bitcoins.chain.models._

import org.bitcoins.core.protocol.blockchain._
import org.bitcoins.rpc.client.common._
import org.bitcoins.testkit.chain._
import org.bitcoins.wallet._
import org.bitcoins.wallet.api._

import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.util._

//the goal for this script is to create a chain and sync it
//to disk after creation

//we should be able to read this chain on subsequent runs
//assuming we are connected to the same bitcoind instance

//you can run this script with
//$ sbt "doc/run doc/src/main/scala/org/bitcoins/doc/chain/sync-chain.sc"


//boring config stuff
val logger = LoggerFactory.getLogger("org.bitcoins.doc.chain.SyncChain")
val time = System.currentTimeMillis()
implicit val system = ActorSystem(s"sync-chain-${time}")
import system.dispatcher

//first we are assuming that a bitcoind regtest node is running in
//the background, you can see 'connect_bitcoind.sc' script
//to see how to bind to a local/remote bitcoind node
//This script assumes that you have a bitcoind instance running in the
//background and that you have ~/.bitcoin/bitcoin.conf setup.
//you need to have 'rpcuser' and 'rpcpassword' set in that bitcoin.conf file
//You can pass in an alternative datadir if you wish by construct a new java.io.File()
val bitcoindInstance = BitcoindInstance.fromDatadir()
val rpcCli = new BitcoindRpcClient(bitcoindInstance)

logger.info(s"Done configuring rpc client")
//next we need to create a way to monitor the chain
val getBestBlockHash = ChainTestUtil.bestBlockHashFnRpc(Future.successful(rpcCli))

val getBlockHeader = ChainTestUtil.getBlockHeaderFnRpc(Future.successful(rpcCli))

val chainDbConfig = ChainDbConfig.RegTestDbConfig
val chainAppConfig = ChainAppConfig(chainDbConfig)

logger.info(s"Creating chain tables")
//initialize chain tables in bitcoin-s if they do not exist
val chainProjectInitF = ChainTestUtil.initializeIfNeeded(chainAppConfig)

val blockHeaderDAO = BlockHeaderDAO(appConfig = chainAppConfig)

val chainHandler = ChainHandler(blockHeaderDAO, chainAppConfig)

val syncedChainApiF = chainProjectInitF.flatMap { _ =>
  logger.info(s"Beginning sync to bitcoin-s chain state")
  ChainSync.sync(chainHandler, getBlockHeader, getBestBlockHash)
}

val syncResultF = syncedChainApiF.flatMap { chainApi =>
  chainApi.getBlockCount.map(count => logger.info(s"chain api blockcount=${count}"))

  rpcCli.getBlockCount.map(count => logger.info(s"bitcoind blockcount=${count}"))
}

syncResultF.onComplete { case result =>

  logger.info(s"Sync result=${result}")
  system.terminate()
}

