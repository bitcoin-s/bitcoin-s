package org.bitcoins.node

import java.net.{InetAddress, InetSocketAddress}

import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderTable}
import org.bitcoins.core.p2p.NetworkIpAddress
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.models.Peer
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object Main extends App with BitcoinSLogger {
  implicit val system = Constants.actorSystem
  import system.dispatcher

  implicit val appconfig = NodeAppConfig()
  implicit val chainAppConfig = ChainAppConfig()
  logger.info(s"Chain config: ${chainAppConfig.dbConfig.config}")

  val bhDAO = BlockHeaderDAO()
  val chainApi = ChainHandler(bhDAO, chainAppConfig)
  val table = TableQuery[BlockHeaderTable]

  logger.info(s"Creating block header table")

  val chainInitF = chainAppConfig.initialize
  Await.result(chainInitF, 3.seconds)
  logger.info(s"Creating block header table: done")

  val socket = new InetSocketAddress(InetAddress.getLoopbackAddress, 18333)
  val nip = NetworkIpAddress.fromInetSocketAddress(socket)
  val peer = Peer(nip)

  logger.info(s"Starting spv node")
  val spvNodeF = SpvNode(peer, chainApi).start()

  logger.info(s"Starting SPV node sync")
  spvNodeF.map { spvNode =>
    spvNode.sync().onComplete {
      case Failure(exception) =>
        logger.error(s"Could not sync SPV node!")
        exception.printStackTrace()
        sys.exit(1)
      case Success(_) =>
        logger.info(s"Started syncing SPV node successfully")
    }
  }
}
