package org.bitcoins.node.config

import com.typesafe.config.Config
import org.apache.pekko.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.CallbackConfig
import org.bitcoins.core.api.node.{NodeType, Peer}
import org.bitcoins.core.api.tor.Socks5ProxyParams
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.db.{DbAppConfig, JdbcProfileComponent}
import org.bitcoins.node._
import org.bitcoins.node.callback.NodeCallbackStreamManager
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.node.util.BitcoinSNodeUtil
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.rpc.util.AppConfigFactoryActorSystem
import org.bitcoins.tor.TorParams
import org.bitcoins.tor.config.TorAppConfig

import java.nio.file.Path
import java.time.{Instant}
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

  /** List of peers hardcoded in our configuration files */
  lazy val peers: Vector[Peer] = {
    val list = config.getStringList("bitcoin-s.node.peers")
    val strs = 0
      .until(list.size())
      .foldLeft(Vector.empty[String])((acc, i) => acc :+ list.get(i))
    val result = strs.map(_.replace("localhost", "127.0.0.1"))

    BitcoinSNodeUtil.stringsToPeers(result)(this)
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
    else 2
  }

  lazy val enablePeerDiscovery: Boolean = {
    if (config.hasPath("bitcoin-s.node.enable-peer-discovery"))
      config.getBoolean("bitcoin-s.node.enable-peer-discovery")
    else false
  }

  lazy val tryPeersStartDelay: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.try-peers-start-delay")) {
      val duration = config.getDuration("bitcoin-s.node.try-peers-start-delay")
      TimeUtil.durationToFiniteDuration(duration)
    } else {
      30.seconds
    }
  }

  /** time interval for trying next set of peers in peer discovery */
  lazy val tryNextPeersInterval: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.try-peers-interval")) {
      val duration = config.getDuration("bitcoin-s.node.try-peers-interval")
      TimeUtil.durationToFiniteDuration(duration)
    } else 12.hour
  }

  /** timeout to wait for response the messages extend [[org.bitcoins.core.p2p.ExpectsResponse]] */
  lazy val queryWaitTime: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.query-wait-time")) {
      val duration = config.getDuration("bitcoin-s.node.query-wait-time")
      TimeUtil.durationToFiniteDuration(duration)
    } else 120.seconds
  }

  /** maximum consecutive number of invalid responses allowed from the same peer */
  lazy val maxInvalidResponsesAllowed: Int = {
    if (config.hasPath("bitcoin-s.node.max-invalid-response-count")) {
      config.getInt("bitcoin-s.node.max-invalid-response-count")
    } else 10
  }

  lazy val healthCheckInterval: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.health-check-interval")) {
      val duration = config.getDuration("bitcoin-s.node.health-check-interval")
      TimeUtil.durationToFiniteDuration(duration)
    } else 1.minute
  }

  lazy val peerTimeout: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.peer-timeout")) {
      val duration = config.getDuration("bitcoin-s.node.peer-timeout")
      TimeUtil.durationToFiniteDuration(duration)
    } else 20.minute
  }

  lazy val connectionAttemptCooldownPeriod: FiniteDuration = {
    if (config.hasPath("bitcoin-s.node.connection-attempt-cool-down-period")) {
      val duration =
        config.getDuration("bitcoin-s.node.connection-attempt-cool-down-period")
      TimeUtil.durationToFiniteDuration(duration)
    } else 5.minute
  }

  /** Creates either a neutrino node or a spv node based on the [[NodeAppConfig]] given */
  def createNode(peers: Vector[Peer], walletCreationTimeOpt: Option[Instant])(
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

    nodeConf.nodeType match {
      case NodeType.NeutrinoNode =>
        val n = NeutrinoNode(walletCreationTimeOpt,
                             nodeConf,
                             chainConf,
                             system,
                             paramPeers = peers)
        Future.successful(n)
      case NodeType.FullNode =>
        Future.failed(new RuntimeException("Not implemented"))
      case NodeType.BitcoindBackend =>
        Future.failed(new RuntimeException("Use a BitcoindRpcClient instead"))
    }
  }
}
