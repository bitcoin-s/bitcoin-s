package org.bitcoins.node

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.models.Peer

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import org.bitcoins.rpc.config.BitcoindInstance
import java.net.InetSocketAddress
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.bloom.BloomUpdateAll

object SpvNodeMain extends App with BitcoinSLogger {
  implicit val system = Constants.actorSystem
  import system.dispatcher

  // TODO: Make BitcoinSAppConfig available in main sources
  // somehow, use this here
  implicit val nodeConf = NodeAppConfig()
  implicit val chainConf = ChainAppConfig()

  val bhDAO = BlockHeaderDAO()
  val chainApi = ChainHandler(bhDAO, chainConf)

  val _ = {
    logger.info(s"Initializing chain and node")

    val initF =
      Future.sequence(List(nodeConf.initialize(), chainConf.initialize()))
    Await.result(initF, 3.seconds)
    logger.info(s"Initializing chain and node: done")
  }

  val peer = {
    val bitcoind = BitcoindInstance.fromDatadir()
    if (bitcoind.network != nodeConf.network) {
      sys.error(
        s"Node (${nodeConf.network}) and bitcoind (${bitcoind.network}) is running on different networks!")
    }

    logger.info(
      s"Connecting to bitcoind running on ${bitcoind.network} at ${bitcoind.uri} ")
    val socket =
      new InetSocketAddress(bitcoind.uri.getHost(), bitcoind.p2pPort)
    Peer(socket)
  }

  logger.info(s"Starting SPV node")

  val emptyBloom =
    BloomFilter(numElements = 1, falsePositiveRate = 1, flags = BloomUpdateAll)
  val spvNodeF = SpvNode(peer, chainApi, emptyBloom).start()

  val getHeight: Runnable = new Runnable {

    def run: Unit =
      spvNodeF
        .flatMap(_.chainApi.getBlockCount)
        .foreach(count => logger.debug(s"SPV block height: $count"))
  }

  val interval = 30.seconds
  system.scheduler.schedule(interval, interval, getHeight)

  spvNodeF.map { spvNode =>
    spvNode.sync().onComplete {
      case Failure(exception) =>
        logger.error(s"Could not sync SPV node!", exception)
        sys.exit(1)
      case Success(_) =>
        logger.info(s"Started syncing SPV node successfully")
    }
  }
}
