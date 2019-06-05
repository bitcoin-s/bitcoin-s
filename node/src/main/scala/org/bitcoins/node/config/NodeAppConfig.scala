package org.bitcoins.node.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import org.bitcoins.node.db.NodeDbManagement
import scala.util.Failure
import scala.util.Success

case class NodeAppConfig(confs: Config*) extends AppConfig {
  override val configOverrides: List[Config] = confs.toList
  override protected def moduleName: String = "node"
  override protected type ConfigType = NodeAppConfig
  override protected def newConfigOfType(configs: List[Config]): NodeAppConfig =
    NodeAppConfig(configs: _*)

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
