package org.bitcoins.node.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import org.bitcoins.node.db.NodeDbManagement
import scala.util.Failure
import scala.util.Success
import java.nio.file.Path

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class NodeAppConfig(
    private val directory: Path,
    private val confs: Config*)
    extends AppConfig {
  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] val moduleName: String = "node"
  override protected[bitcoins] type ConfigType = NodeAppConfig
  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): NodeAppConfig =
    NodeAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

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

object NodeAppConfig {

  /** Constructs a node configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(confs: Config*): NodeAppConfig =
    NodeAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs: _*)

}
