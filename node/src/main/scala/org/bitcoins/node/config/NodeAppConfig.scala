package org.bitcoins.node.config

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.chain.blockchain.ChainHandlerCached
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.util.Mutable
import org.bitcoins.db.{AppConfigFactory, DbAppConfig, JdbcProfileComponent}
import org.bitcoins.node._
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.tor.Socks5ProxyParams

import java.net.InetSocketAddress
import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class NodeAppConfig(
    private val directory: Path,
    private val confs: Config*)(implicit override val ec: ExecutionContext)
    extends DbAppConfig
    with NodeDbManagement
    with JdbcProfileComponent[NodeAppConfig] {
  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] def moduleName: String = NodeAppConfig.moduleName
  override protected[bitcoins] type ConfigType = NodeAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): NodeAppConfig =
    NodeAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def appConfig: NodeAppConfig = this

  private val callbacks = new Mutable(NodeCallbacks.empty)

  def nodeCallbacks: NodeCallbacks = callbacks.atomicGet

  def addCallbacks(newCallbacks: NodeCallbacks): NodeCallbacks = {
    callbacks.atomicUpdate(newCallbacks)(_ + _)
  }

  /** Ensures correct tables and other required information is in
    * place for our node.
    */
  override def start(): Future[Unit] = {
    for {
      _ <- super.start()
    } yield {
      logger.debug(s"Initializing node setup")
      val numMigrations = migrate()
      val _ = if (isHikariLoggingEnabled) {
        //.get is safe because hikari logging is enabled
        startHikariLogger(hikariLoggingInterval.get)
        ()
      } else {
        ()
      }
      logger.info(s"Applied $numMigrations migrations fro the node project")
    }
  }

  override def stop(): Future[Unit] = {
    val _ = stopHikariLogger()
    super.stop()
  }

  lazy val nodeType: NodeType =
    NodeType.fromString(config.getString("bitcoin-s.node.mode"))

  /** List of peers
    */
  lazy val peers: Vector[String] = {
    val list = config.getStringList("bitcoin-s.node.peers")
    val strs = 0
      .until(list.size())
      .foldLeft(Vector.empty[String])((acc, i) => acc :+ list.get(i))
    strs.map(_.replace("localhost", "127.0.0.1"))
  }

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] = {
    if (config.getBoolean("bitcoin-s.proxy.enabled")) {
      Some(
        Socks5ProxyParams(
          address = InetSocketAddress.createUnresolved(
            config.getString("bitcoin-s.proxy.host"),
            config.getInt("bitcoin-s.proxy.port")
          ),
          credentialsOpt = None,
          randomizeCredentials = true
        )
      )
    } else {
      None
    }
  }

  lazy val relay: Boolean = {
    if (config.hasPath("bitcoin-s.node.relay")) {
      config.getBoolean("bitcoin-s.node.relay")
    } else {
      false
    }
  }

  /** Creates either a neutrino node or a spv node based on the [[NodeAppConfig]] given */
  def createNode(peer: Peer)(
      chainConf: ChainAppConfig,
      system: ActorSystem): Future[Node] = {
    NodeAppConfig.createNode(peer)(this, chainConf, system)
  }
}

object NodeAppConfig extends AppConfigFactory[NodeAppConfig] {

  override val moduleName: String = "node"

  /** Constructs a node configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): NodeAppConfig =
    NodeAppConfig(datadir, confs: _*)

  /** Creates either a neutrino node or a spv node based on the [[NodeAppConfig]] given */
  def createNode(peer: Peer)(implicit
      nodeConf: NodeAppConfig,
      chainConf: ChainAppConfig,
      system: ActorSystem): Future[Node] = {
    import system.dispatcher

    val blockHeaderDAO = BlockHeaderDAO()
    val filterHeaderDAO = CompactFilterHeaderDAO()
    val filterDAO = CompactFilterDAO()

    val dmhF = ChainHandlerCached
      .fromDatabase(blockHeaderDAO, filterHeaderDAO, filterDAO)
      .map(handler => DataMessageHandler(handler))

    nodeConf.nodeType match {
      case NodeType.SpvNode =>
        dmhF.map(dmh => SpvNode(peer, dmh, nodeConf, chainConf, system))
      case NodeType.NeutrinoNode =>
        dmhF.map(dmh => NeutrinoNode(peer, dmh, nodeConf, chainConf, system))
      case NodeType.FullNode =>
        Future.failed(new RuntimeException("Not implemented"))
      case NodeType.BitcoindBackend =>
        Future.failed(new RuntimeException("Use a BitcoindRpcClient instead"))
    }
  }
}
