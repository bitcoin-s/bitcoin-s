package org.bitcoins.node.config

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.chain.blockchain.ChainHandlerCached
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  ChainStateDescriptorDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.CallbackConfig
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.config.{MainNet, RegTest, SigNet, TestNet3}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.db.{DbAppConfig, JdbcProfileComponent}
import org.bitcoins.node._
import org.bitcoins.node.callback.NodeCallbackStreamManager
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.node.models.Peer
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.rpc.util.AppConfigFactoryActorSystem
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.nio.file.Path
import java.time.{Duration, Instant}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class NodeAppConfig(baseDatadir: Path, configOverrides: Vector[Config])(
    implicit val system: ActorSystem)
    extends DbAppConfig
    with NodeDbManagement
    with JdbcProfileComponent[NodeAppConfig]
    with CallbackConfig[NodeCallbacks] {
  override protected[bitcoins] def moduleName: String = NodeAppConfig.moduleName
  override protected[bitcoins] type ConfigType = NodeAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Vector[Config]): NodeAppConfig =
    NodeAppConfig(baseDatadir, configs)

  implicit override def ec: ExecutionContext = system.dispatcher

  override def appConfig: NodeAppConfig = this

  override lazy val callbackFactory: NodeCallbacks.type = NodeCallbacks

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
          case NodeType.NeutrinoNode | NodeType.FullNode =>
            Future.unit
        }
      }
    } yield {
      logger.info(s"Initializing node setup")
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
    val stopCallbacksF = callBacks match {
      case stream: NodeCallbackStreamManager => stream.stop()
      case _: NodeCallbacks =>
        Future.unit
    }
    val _ = stopHikariLogger()
    stopCallbacksF.flatMap(_ => super.stop())
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
    val result = strs.map(_.replace("localhost", "127.0.0.1"))
    if (result.isEmpty && useDefaultPeers) {
      logger.info(
        s"No peers found in configuration, resorting to default peers")
      network match {
        case MainNet  => Vector("neutrino.suredbits.com:8333")
        case TestNet3 => Vector("neutrino.testnet3.suredbits.com:18333")
        case n @ (RegTest | SigNet) =>
          sys.error(s"Cannot configure any peers by default on $n")
      }
    } else {
      result
    }
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

  lazy val enablePeerDiscovery: Boolean = {
    if (config.hasPath("bitcoin-s.node.enable-peer-discovery"))
      config.getBoolean("bitcoin-s.node.enable-peer-discovery")
    else false
  }

  // https://github.com/lightbend/config/blob/master/HOCON.md#duration-format
  lazy val peerDiscoveryTimeout: Duration = {
    if (config.hasPath("bitcoin-s.node.peer-discovery-timeout"))
      config.getDuration("bitcoin-s.node.peer-discovery-timeout")
    else Duration.ofMinutes(10)
  }

  lazy val useDefaultPeers: Boolean = {
    if (config.hasPath("bitcoin-s.node.use-default-peers"))
      config.getBoolean("bitcoin-s.node.use-default-peers")
    else true
  }

  /** timeout for tcp connection in P2PClient */
  lazy val connectionTimeout: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.connection-timeout")) {
      val duration = config.getDuration("bitcoin-s.node.connection-timeout")
      TimeUtil.durationToFiniteDuration(duration)
    } else 5.seconds
  }

  /** initialization timeout once connected, reconnections reset this */
  lazy val initializationTimeout: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.initialization-timeout")) {
      val duration = config.getDuration("bitcoin-s.node.initialization-timeout")
      TimeUtil.durationToFiniteDuration(duration)
    } else 10.seconds
  }

  /** time interval for trying next set of peers in peer discovery */
  lazy val tryNextPeersInterval: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.try-peers-interval")) {
      val duration = config.getDuration("bitcoin-s.node.try-peers-interval")
      TimeUtil.durationToFiniteDuration(duration)
    } else 12.seconds
  }

  /** timeout to wait for response the messages extend [[org.bitcoins.core.p2p.ExpectsResponse]] */
  lazy val queryWaitTime: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.query-wait-time")) {
      val duration = config.getDuration("bitcoin-s.node.query-wait-time")
      TimeUtil.durationToFiniteDuration(duration)
    } else 15.seconds
  }

  /** maximum consecutive number of invalid responses allowed from the same peer */
  lazy val maxInvalidResponsesAllowed: Int = {
    if (config.hasPath("bitcoin-s.node.max-invalid-response-count")) {
      config.getInt("bitcoin-s.node.max-invalid-response-count")
    } else 10
  }

  /** Creates either a neutrino node or a spv node based on the [[NodeAppConfig]] given */
  def createNode(
      peers: Vector[Peer] = Vector.empty[Peer],
      walletCreationTimeOpt: Option[Instant])(
      chainConf: ChainAppConfig,
      system: ActorSystem): Future[Node] = {
    NodeAppConfig.createNode(peers, walletCreationTimeOpt)(this,
                                                           chainConf,
                                                           system)
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
  def createNode(peers: Vector[Peer], walletCreationTimeOpt: Option[Instant])(
      implicit
      nodeConf: NodeAppConfig,
      chainConf: ChainAppConfig,
      system: ActorSystem): Future[Node] = {
    import system.dispatcher

    val blockHeaderDAO = BlockHeaderDAO()
    val filterHeaderDAO = CompactFilterHeaderDAO()
    val filterDAO = CompactFilterDAO()
    val stateDAO = ChainStateDescriptorDAO()

    val chainF = ChainHandlerCached
      .fromDatabase(blockHeaderDAO, filterHeaderDAO, filterDAO, stateDAO)

    nodeConf.nodeType match {
      case NodeType.NeutrinoNode =>
        chainF.map(chain =>
          NeutrinoNode(chain,
                       walletCreationTimeOpt,
                       nodeConf,
                       chainConf,
                       system,
                       paramPeers = peers))
      case NodeType.FullNode =>
        Future.failed(new RuntimeException("Not implemented"))
      case NodeType.BitcoindBackend =>
        Future.failed(new RuntimeException("Use a BitcoindRpcClient instead"))
    }
  }
}
