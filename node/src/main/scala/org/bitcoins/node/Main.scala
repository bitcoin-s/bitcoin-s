package org.bitcoins.node

import slick.jdbc.SQLiteProfile.api._
import org.bitcoins.chain.models.BlockHeaderTable
import scala.concurrent.duration._
import scala.concurrent._
import org.bitcoins.node.constant.Constants
import akka.actor.ActorSystem
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.blockchain.ChainHandler
import java.net.InetSocketAddress
import org.bitcoins.node.util.NetworkIpAddress
import org.bitcoins.node.models.Peer
import scala.util.Failure
import org.bitcoins.core.util.BitcoinSLogger
import scala.util.Success
import java.io.PrintWriter
import java.io.PrintStream
import java.io.OutputStream

object Main extends App with BitcoinSLogger {
  implicit val system = Constants.actorSystem
  import system.dispatcher

  implicit val appconfig = NodeAppConfig
  val chainAppConfig = ChainAppConfig
  logger.info(s"Chain config: ${chainAppConfig.dbConfig.config}")

  val bhDAO = BlockHeaderDAO(chainAppConfig)
  val chainApi = ChainHandler(bhDAO, chainAppConfig)
  val table = TableQuery[BlockHeaderTable]

  logger.info(s"Creating block header table")

  val createTablesF =
    ChainAppConfig.database.run(table.schema.createIfNotExists)
  Await.result(createTablesF, 3.seconds)
  logger.info(s"Creating block header table: done")

  val socket = new InetSocketAddress("localhost", 18333)
  val nip = NetworkIpAddress.fromInetSocketAddress(socket)
  val peer = Peer(nip)
  val spvNode = SpvNode(peer, chainApi)

  logger.info(s"Starting SPV node sync")
  spvNode.sync().onComplete {
    case Failure(exception) =>
      logger.error(s"Could not sync SPV node!")
      exception.printStackTrace()
      sys.exit(1)
    case Success(_) => logger.info(s"Synced SPV node successfully")
  }
}
