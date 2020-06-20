package org.bitcoins.node.config

import java.nio.file.Path

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.{AppConfig, AppConfigFactory, JdbcProfileComponent}
import org.bitcoins.node.{NeutrinoNode, Node, SpvNode}
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.node.models.Peer

import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class NodeAppConfig(
    private val directory: Path,
    override val useLogbackConf: Boolean,
    private val confs: Config*)(implicit override val ec: ExecutionContext)
    extends AppConfig
    with NodeDbManagement
    with JdbcProfileComponent[NodeAppConfig] {
  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] def moduleName: String = "node"
  override protected[bitcoins] type ConfigType = NodeAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): NodeAppConfig =
    NodeAppConfig(directory, useLogbackConf, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def appConfig: NodeAppConfig = this

  /**
    * Ensures correct tables and other required information is in
    * place for our node.
    */
  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    logger.debug(s"Initializing node setup")
    val numMigrations = migrate()

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

  /** Starts the associated application */
  override def start(): Future[Unit] = FutureUtil.unit

  /** Creates either a neutrino node or a spv node based on the [[NodeAppConfig]] given */
  def createNode(peer: Peer)(
      chainConf: ChainAppConfig,
      system: ActorSystem): Future[Node] = {
    NodeAppConfig.createNode(peer)(this, chainConf, system)
  }
}

object NodeAppConfig extends AppConfigFactory[NodeAppConfig] {

  /** Constructs a node configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(
      datadir: Path,
      useLogbackConf: Boolean,
      confs: Vector[Config])(implicit ec: ExecutionContext): NodeAppConfig =
    NodeAppConfig(datadir, useLogbackConf, confs: _*)

  /** Creates either a neutrino node or a spv node based on the [[NodeAppConfig]] given */
  def createNode(peer: Peer)(implicit
      nodeConf: NodeAppConfig,
      chainConf: ChainAppConfig,
      system: ActorSystem): Future[Node] = {
    if (nodeConf.isSPVEnabled) {
      Future.successful(SpvNode(peer, nodeConf, chainConf, system))
    } else if (nodeConf.isNeutrinoEnabled) {
      Future.successful(NeutrinoNode(peer, nodeConf, chainConf, system))
    } else {
      Future.failed(
        new RuntimeException("Neither Neutrino nor SPV mode is enabled."))
    }
  }
}
