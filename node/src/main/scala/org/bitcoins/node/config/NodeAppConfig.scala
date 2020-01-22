package org.bitcoins.node.config

import java.nio.file.Path

import com.typesafe.config.Config
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.AppConfig
import org.bitcoins.node.db.NodeDbManagement

import scala.concurrent.{ExecutionContext, Future}

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
    val numMigrations = NodeDbManagement.migrate(this)

    logger.info(s"Applied $numMigrations migrations fro the node project")

    FutureUtil.unit
  }

  /**
    * Whether or not SPV (simplified payment verification)
    * mode is enabled.
    */
  lazy val isSPVEnabled: Boolean = config
    .getString("node.mode")
    .toLowerCase == "spv"

  /**
    * Whether or not Neutrino (compact block filters) mode
    * is enabled
    */
  lazy val isNeutrinoEnabled: Boolean = config
    .getString("node.mode")
    .toLowerCase == "neutrino"

  /**
    * List of peers
    */
  lazy val peers: Vector[String] = {
    val list = config.getStringList("node.peers")
    0.until(list.size())
      .foldLeft(Vector.empty[String])((acc, i) => acc :+ list.get(i))
  }
}

object NodeAppConfig {

  /** Constructs a node configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(confs: Config*): NodeAppConfig =
    NodeAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs: _*)

}
