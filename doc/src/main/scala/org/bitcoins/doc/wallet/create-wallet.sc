import java.io.File

import org.bitcoins.chain.blockchain.{Blockchain, ChainHandler}
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.protocol.blockchain.{Block, RegTestNetChainParams}
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.api.InitializeWalletSuccess
import scodec.bits.ByteVector
import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.db.ChainDbConfig
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.blockchain.sync.ChainSync
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.number._
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.db.WalletDbConfig
import org.bitcoins.wallet.config.WalletAppConfig

import org.bitcoins.zmq.ZMQSubscriber
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.concurrent._
import scala.concurrent.duration.DurationInt
import scala.util._
/**
* This is for example purposes only!
  * This shows how to peer a bitcoin-s wallet
  * with a bitcoind instance that is relaying
  * information about what is happening on the blockchain
  * to the bitcoin-s wallet.
  *
  * This is useful if you want more flexible signing
  * procedures in the JVM ecosystem and more
  * granular control over your utxos with
  * popular databases like postgres, sqlite etc
  */

//you can run this script with the following command
//$ sbt "doc/run doc/src/main/scala/org/bitcoins/doc/wallet/create-wallet.sc"

val logger = LoggerFactory.getLogger("org.bitcoins.doc.wallet.CreateWallet")
val time = System.currentTimeMillis()
//boiler plate config
implicit val system = ActorSystem(s"wallet-scala-sheet-${time}")
import system.dispatcher

val chainDbConfig = ChainDbConfig.RegTestDbConfig
val chainAppConfig = ChainAppConfig(chainDbConfig)
implicit val chainParams = chainAppConfig.chain

val walletDbConfig = WalletDbConfig.RegTestDbConfig
val walletAppConfig = WalletAppConfig(walletDbConfig)

val datadir = new File(s"/tmp/bitcoin-${time}/")
val bitcoinConf = new File(datadir.getAbsolutePath + "/bitcoin.conf")

logger.info(s"bitcoin.conf location=${bitcoinConf.getAbsolutePath}")
datadir.mkdirs()
bitcoinConf.createNewFile()

val config = BitcoindRpcTestUtil.standardConfig
val _ = BitcoindRpcTestUtil.writeConfigToFile(config,datadir)

//construct bitcoind
val instance = BitcoindInstance.fromConfig(config = config, datadir)
val bitcoind = new BitcoindRpcClient(instance = instance)

//start bitcoind, this may take a little while
//generate 101 blocks so we have money in our wallet
val bitcoindF = bitcoind.start().map(_ => bitcoind)

//create a native chain handler for bitcoin-s
val blockHeaderDAO: BlockHeaderDAO = BlockHeaderDAO(appConfig = chainAppConfig)
val genesisHeader = BlockHeaderDbHelper.fromBlockHeader(
  height = 0,
  bh = chainAppConfig.chain.genesisBlock.blockHeader)


val blockHeaderTableF = {
  //drop regtest table if it exists
  val dropTableF = ChainDbManagement.dropHeaderTable(chainDbConfig)

  //recreate the table
  val createdTableF = dropTableF.flatMap(_ => ChainDbManagement.createHeaderTable(chainDbConfig))

  createdTableF
}
val createdGenHeaderF = blockHeaderTableF.flatMap(_ => blockHeaderDAO.create(genesisHeader))

val chainF = createdGenHeaderF.map(h => Vector(h))

val blockchainF = chainF.map(chain => Blockchain(chain))

val chainHandlerF = blockchainF.map(blockchain => ChainHandler(blockHeaderDAO, chainAppConfig))

val chainApi101BlocksF = sync(chainHandlerF, 101)

val bitcoinsLogF = chainApi101BlocksF.flatMap { chainApi =>
  chainApi.getBlockCount.map(count => logger.info(s"bitcoin-s blockcount=${count}"))
}

val walletF = bitcoinsLogF.flatMap { _ =>
  //create tables
  val dropTablesF = WalletDbManagement.dropAll(walletDbConfig)
  val createTablesF = dropTablesF.flatMap(_ => WalletDbManagement.createAll(walletDbConfig))
  createTablesF.flatMap { _ =>
    Wallet.initialize(walletAppConfig)
      .collect{ case success: InitializeWalletSuccess => success.wallet }
  }
}

val bitcoinsAddrF = walletF.flatMap(_.getNewAddress())

//send money to our wallet with bitcoind
val amt = Bitcoins.one
val transactionOutputIndexF: Future[(Transaction,Int)] = for {
  bitcoind <- bitcoindF
  bitcoinsAddr <- bitcoinsAddrF
  txid <- bitcoind.sendToAddress(bitcoinsAddr, amt)
  tx <- bitcoind.getRawTransactionRaw(txid)
} yield {
  logger.info(s"Sending ${amt} to address ${bitcoinsAddr.value}")
  val Some((output,index)) = tx.outputs.zipWithIndex.find { case (output,index) =>
    output.scriptPubKey == bitcoinsAddr.scriptPubKey
  }

  (tx,index)
}

//add the utxo that was just created by bitcoind to our wallet
val addUtxoF = for {
  wallet <- walletF
  (tx,index) <- transactionOutputIndexF
  addUtxo <- wallet.addUtxo(tx,UInt32(index))
} yield {
  logger.info(s"Add utxo result=${addUtxo}")
  addUtxo
}

//bury the utxo with enough proof of work to make it confirmed
val chainApi6BlocksF = for {
  addUtxo <- addUtxoF
  (tx,_) <- transactionOutputIndexF
  chainApi <- sync(chainApi101BlocksF,6)
} yield {
  logger.info(s"txid=${tx.txId.flip.hex}")
}

//check balance & clean everything up
chainApi6BlocksF.onComplete { chainApi =>
  val balanceF = walletF.flatMap(_.getBalance)

  balanceF.onComplete(balance => logger.info(s"bitcoin-s walllet balance=${balance}"))

  balanceF.flatMap(_ => cleanup())
}



/** Syncs the give number of blocks to our chain */
def sync(chainHandlerF: Future[ChainApi], numBlocks: Int)(implicit ec: ExecutionContext): Future[ChainApi] = {
  //we need a way to connect bitcoin-s to our running bitcoind, we are going to do this via rpc for now
  //we need to implement the 'getBestBlockHashFunc' and 'getBlockHeaderFunc' functions
  //to be able to sync our internal bitcoin-s chain with our external bitcoind chain
  val getBestBlockHashFunc = { () =>
    bitcoindF.flatMap(_.getBestBlockHash)
  }

  val getBlockHeaderFunc = { hash: DoubleSha256DigestBE =>
    bitcoindF.flatMap(_.getBlockHeader(hash).map(_.blockHeader))
  }


  //now that we have bitcoind setup correctly and have rpc linked to
  //the bitcoin-s chain project, let's generate some blocks so
  //we have money to spend in our bitcoind wallet!
  //we need to generate 101 blocks to give us 50 btc to spend
  val genBlocksF = chainHandlerF.flatMap { _ =>
    bitcoindF.flatMap(_.generate(numBlocks))
  }

  //now we need to sync those blocks into bitcoin-s
  val chainSyncF = genBlocksF.flatMap { _ =>
    chainHandlerF.flatMap { ch =>
      ChainSync.sync(
        ch.asInstanceOf[ChainHandler],
        getBlockHeaderFunc,
        getBestBlockHashFunc)
    }
  }

  chainSyncF
}

def cleanup(): Future[Unit] = {
  logger.info("Beginning clean up of create wallet script")
  val bitcoindStopF = {
    bitcoindF.flatMap { bitcoind =>
      val stopF = bitcoind.stop()
      stopF
    }
  }
  datadir.delete()
  logger.debug("cleaning up chain, wallet, and system")
  val chainCleanupF = ChainDbManagement.dropAll(chainDbConfig)
  val walletCleanupF = WalletDbManagement.dropAll(walletDbConfig)

  val doneWithCleanupF = for {
    _ <- bitcoindStopF
    _ <- chainCleanupF
    _ <- walletCleanupF
    _ <- system.terminate()
  } yield {
    logger.info(s"Done cleaning up")
  }

  doneWithCleanupF
}


