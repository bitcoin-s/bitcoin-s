import java.io.File

import org.bitcoins.chain.blockchain.{Blockchain, ChainHandler}
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.protocol.blockchain.{Block, RegTestNetChainParams}
import org.bitcoins.db.RegTestDbConfig
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.api.InitializeWalletSuccess
import scodec.bits.ByteVector
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.wallet.config.WalletDbManagement
import org.bitcoins.zmq.ZMQSubscriber
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
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


val logger = LoggerFactory.getLogger("org.bitcoins.doc.wallet.CreateWallet")
val time = System.currentTimeMillis()
//boiler plate config
implicit val system = ActorSystem(s"wallet-scala-sheet-${time}")
import system.dispatcher

//network/db configuration
val dbConfig = RegTestDbConfig
val chainParams = RegTestNetChainParams

val datadir = new File(s"/tmp/bitcoin-${time}/")
val bitcoinConf = new File(datadir.getAbsolutePath + "/bitcoin.conf")
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
val blockHeaderDAO: BlockHeaderDAO = BlockHeaderDAO(chainParams = chainParams,dbConfig = dbConfig)
val genesisHeader = BlockHeaderDbHelper.fromBlockHeader(
  height = 0,
  bh = chainParams.genesisBlock.blockHeader)
val blockHeaderTableF = {
  //drop regtest table if it exists
  val dropTableF = ChainDbManagement.dropHeaderTable(dbConfig)

  //recreate the table
  val createdTableF = dropTableF.flatMap(_ => ChainDbManagement.createHeaderTable(dbConfig))

  createdTableF
}
val createdGenHeaderF = blockHeaderTableF.flatMap(_ => blockHeaderDAO.create(genesisHeader))
val chainF = createdGenHeaderF.map(h => Vector(h))
val blockchainF = chainF.map(chain => Blockchain(chain,blockHeaderDAO))
val chainHandlerF = blockchainF.map(blockchain => ChainHandler(blockchain))

//we need a way to connect bitcoin-s to our running bitcoind, let's do this with ZMQ for now
val zmqBlockUri = instance.zmqConfig.rawBlock.get
val handleBlock = { bytes: ByteVector =>
  val block = Block.fromBytes(bytes)
  logger.info(s"Received block=${block.blockHeader.hashBE.hex}")
  val resultF = chainHandlerF.flatMap(_.processHeader(block.blockHeader))
  resultF.onComplete(result => logger.info(s"Process header result=${result}"))
  ()
}

val zmqStartF = bitcoindF.map { _ =>
  val zmqSub = new ZMQSubscriber(zmqBlockUri, None, None, None, Some(handleBlock))
  zmqSub.start()
  //takes awhile for zmq to start...
  Thread.sleep(1000)
}


//now that we have bitcoind setup correctly and have zmq linked to
//the bitcoin-s chain project, let's generate some blocks so
//we have money to spend in our bitcoind wallet!
//we need to generate 101 blocks to give us 50 btc to spend
val genBlocksF = zmqStartF.flatMap { _ =>
  bitcoindF.flatMap(_.generate(101))
}

val bitcoinsLogF = genBlocksF.map { genBlocks =>
  Thread.sleep(5000)
  val countF = chainHandlerF.flatMap(_.getBlockCount)
  genBlocks
}
val walletF = bitcoinsLogF.flatMap { _ =>
  //create tables
  val createTablesF = WalletDbManagement.createAll(dbConfig)
  createTablesF.flatMap { _ =>
    Wallet.initialize(chainParams, dbConfig)
      .map(_.asInstanceOf[InitializeWalletSuccess].wallet)
  }
}

val addressF = walletF.flatMap(_.getNewAddress())

//send money to our wallet with bitcoind

//clean everything up
addressF.onComplete { case _ =>

  Thread.sleep(5000)
  logger.info("Beginning clean up of create wallet script")
  val bitcoindStopF = {
    bitcoindF.flatMap { bitcoind =>
      val stopF = bitcoind.stop()
      stopF
    }
  }
  datadir.delete()
  //zmqSub.stop()
  logger.debug("cleaning up chain, wallet, and system")
  val chainCleanupF = ChainDbManagement.dropAll(dbConfig)
  val walletCleanupF = WalletDbManagement.dropAll(dbConfig)
  val systemTermF = system.terminate()

  for {
    _ <- bitcoindStopF
    _ <- chainCleanupF
    _ <- walletCleanupF
    _ <- systemTermF
  } yield {
    logger.info(s"Done cleaning up")
  }

}