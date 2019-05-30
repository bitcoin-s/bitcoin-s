package org.bitcoins.node.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig
import org.bitcoins.node.db.NodeDbManagement

import scala.concurrent.{ExecutionContext, Future}
import java.nio.file.Path
import scala.util.Failure
import scala.util.Success

case class NodeAppConfig(confs: Config*) extends AppConfig {
  override val configOverrides: List[Config] = confs.toList
  override protected def moduleConfigName: String = "node.conf"
  override protected type ConfigType = NodeAppConfig
  override protected def newConfigOfType(configs: List[Config]): NodeAppConfig =
    NodeAppConfig(configs: _*)

  /** The desirable false positive rate for our bloom filter */
  lazy val bloomFalsePositiveRate: Double =
    config.getDouble("node.bloomFalsePositiveRate")

  /** Whether or not we should dump P2P messages to file  */
  lazy val dumpP2PBytesToFile: Boolean =
    config.getBoolean("node.p2p.dumpRawBytes")

  /** The file we dump our P2P messages to */
  lazy val p2pDumpFile: Path = datadir.resolve("p2p.dump")

  /**
    * Ensures correct tables and other required information is in
    * place for our node.
    */
  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    logger.debug(s"Initializing node setup")
    val initF = NodeDbManagement.createAll()(config = this, ec)
    initF.onComplete {
      case Failure(err) =>
        logger.error(s"Error when initializing node: ${err.getMessage}")
      case Success(_) =>
        logger.debug(s"Initializing node setup: done")
    }
    initF
  }

}
