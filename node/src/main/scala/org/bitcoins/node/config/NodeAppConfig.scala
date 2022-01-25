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
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.util.Mutable
import org.bitcoins.db.{DbAppConfig, JdbcProfileComponent}
import org.bitcoins.node._
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.DataMessageHandler
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.rpc.util.AppConfigFactoryActorSystem
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class NodeAppConfig(baseDatadir: Path, configOverrides: Vector[Config])(
    implicit val system: ActorSystem)
    extends DbAppConfig
    with NodeDbManagement
    with JdbcProfileComponent[NodeAppConfig] {
  override protected[bitcoins] def moduleName: String = NodeAppConfig.moduleName
  override protected[bitcoins] type ConfigType = NodeAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): NodeAppConfig =
    NodeAppConfig(baseDatadir, configs.toVector)

  implicit override def ec: ExecutionContext = system.dispatcher

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
      _ <- {
        nodeType match {
          case NodeType.BitcoindBackend =>
            val bitcoindRpcAppConfig =
              BitcoindRpcAppConfig(baseDatadir, configOverrides)(system)
            bitcoindRpcAppConfig.binaryOpt match {
              case Some(_) =>
                bitcoindRpcAppConfig.clientF
                  .flatMap(_.start())
                  .map(_ => ())
              case None =>
                Future.unit
            }
          case NodeType.SpvNode | NodeType.NeutrinoNode | NodeType.FullNode =>
            Future.unit
        }
      }
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

  lazy val torConf: TorAppConfig =
    TorAppConfig(baseDatadir, Some(moduleName), configOverrides)

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] =
    torConf.socks5ProxyParams

  lazy val torParams: Option[TorParams] = torConf.torParams

  lazy val relay: Boolean = {
    if (config.hasPath("bitcoin-s.node.relay")) {
      config.getBoolean("bitcoin-s.node.relay")
    } else {
      false
    }
  }

  lazy val maxConnectedPeers: Int = {
    if (config.hasPath("bitcoin-s.node.maxConnectedPeers"))
      config.getInt("bitcoin-s.node.maxConnectedPeers")
    else 1
  }

  /** Creates either a neutrino node or a spv node based on the [[NodeAppConfig]] given */
  def createNode(peers: Vector[Peer] = Vector.empty[Peer])(
      chainConf: ChainAppConfig,
      system: ActorSystem): Future[Node] = {
    NodeAppConfig.createNode(peers)(this, chainConf, system)
  }
}

object NodeAppConfig extends AppConfigFactoryActorSystem[NodeAppConfig] {

  override val moduleName: String = "node"

  /** Constructs a node configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      system: ActorSystem): NodeAppConfig =
    NodeAppConfig(datadir, confs)

  /** Creates either a neutrino node or a spv node based on the [[NodeAppConfig]] given */
  def createNode(peers: Vector[Peer])(implicit
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
        dmhF.map(dmh =>
          SpvNode(dmh,
                  nodeConf,
                  chainConf,
                  system,
                  configPeersOverride = peers))
      case NodeType.NeutrinoNode =>
        dmhF.map(dmh =>
          NeutrinoNode(dmh,
                       nodeConf,
                       chainConf,
                       system,
                       configPeersOverride = peers))
      case NodeType.FullNode =>
        Future.failed(new RuntimeException("Not implemented"))
      case NodeType.BitcoindBackend =>
        Future.failed(new RuntimeException("Use a BitcoindRpcClient instead"))
    }
  }
}
