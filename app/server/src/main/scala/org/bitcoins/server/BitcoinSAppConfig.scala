package org.bitcoins.server

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import grizzled.slf4j.Logging
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.commons.config.{AppConfig, ConfigOps}
import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.{StartStopAsync, TimeUtil}
import org.bitcoins.dlc.node.config.DLCNodeAppConfig
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.keymanager.config.KeyManagerAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.wallet.config.WalletAppConfig

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/** A unified config class for all submodules of Bitcoin-S
  * that accepts configuration. Thanks to implicit definitions
  * in this case class' companion object an instance
  * of this class can be passed in anywhere a wallet,
  * chain or node config is required.
  *
  * @param directory The data directory of this app configuration
  * @param confs A sequence of optional configuration overrides
  */
case class BitcoinSAppConfig(
    baseDatadir: Path,
    configOverrides: Vector[Config])(implicit system: ActorSystem)
    extends StartStopAsync[Unit]
    with Logging {
  import system.dispatcher

  lazy val walletConf: WalletAppConfig =
    WalletAppConfig(baseDatadir, configOverrides)
  lazy val nodeConf: NodeAppConfig = NodeAppConfig(baseDatadir, configOverrides)

  lazy val chainConf: ChainAppConfig =
    ChainAppConfig(baseDatadir, configOverrides)
  lazy val dlcConf: DLCAppConfig = DLCAppConfig(baseDatadir, configOverrides)

  lazy val torConf: TorAppConfig =
    TorAppConfig(baseDatadir, None, configOverrides)

  lazy val dlcNodeConf: DLCNodeAppConfig =
    DLCNodeAppConfig(baseDatadir, configOverrides)

  def copyWithConfig(newConfs: Vector[Config]): BitcoinSAppConfig = {
    val configs = newConfs ++ configOverrides
    BitcoinSAppConfig(baseDatadir, configs)
  }

  lazy val kmConf: KeyManagerAppConfig =
    KeyManagerAppConfig(baseDatadir, configOverrides)

  lazy val bitcoindRpcConf: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig(baseDatadir, configOverrides)

  lazy val network: NetworkParameters = chainConf.network

  /** Initializes the wallet, node and chain projects */
  override def start(): Future[Unit] = {
    val start = TimeUtil.currentEpochMs
    //configurations that don't depend on tor startup
    //start these in parallel as an optimization
    val nonTorConfigs = Vector(kmConf, chainConf, walletConf)

    val torConfig = torConf.start()
    val torDependentConfigs = Vector(nodeConf, bitcoindRpcConf, dlcConf)

    val startedTorDependentConfigsF = for {
      _ <- torConfig
      _ <- Future.sequence(torDependentConfigs.map(_.start()))
    } yield ()

    val startedNonTorConfigs = Future.sequence(nonTorConfigs.map(_.start()))

    for {
      _ <- startedNonTorConfigs
      _ <- startedTorDependentConfigsF
    } yield {
      logger.info(
        s"Done starting BitcoinSAppConfig, it took=${TimeUtil.currentEpochMs - start}ms")
      ()
    }
  }

  override def stop(): Future[Unit] = {
    for {
      _ <- nodeConf.stop()
      _ <- walletConf.stop()
      _ <- chainConf.stop()
      _ <- bitcoindRpcConf.stop()
      _ <- torConf.stop()
    } yield ()
  }

  /** The underlying config the result of our fields derive from */
  lazy val config: Config = {
    val finalConfig =
      AppConfig.getBaseConfig(baseDatadir = baseDatadir, configOverrides)
    val resolved = finalConfig.resolve()

    resolved.checkValid(ConfigFactory.defaultReference(), "bitcoin-s")

    resolved
  }

  def rpcPort: Int = config.getInt("bitcoin-s.server.rpcport")

  def wsPort: Int = config.getIntOrElse("bitcoin-s.server.wsport", 19999)

  /** How long until we forcefully terminate connections to the server
    * when shutting down the server
    */
  def terminationDeadline: FiniteDuration = {
    val opt = config.getDurationOpt("bitcoin-s.server.termination-deadline")
    opt match {
      case Some(duration) =>
        if (duration.isFinite) {
          new FiniteDuration(duration.toNanos, TimeUnit.NANOSECONDS)
        } else {
          sys.error(
            s"Can only have a finite duration for termination deadline, got=$duration")
        }
      case None => 5.seconds //5 seconds by default
    }
  }

  def rpcBindOpt: Option[String] = {
    if (config.hasPath("bitcoin-s.server.rpcbind")) {
      Some(config.getString("bitcoin-s.server.rpcbind"))
    } else {
      None
    }
  }

  def wsBindOpt: Option[String] = {
    if (config.hasPath("bitcoin-s.server.wsbind")) {
      Some(config.getString("bitcoin-s.server.wsbind"))
    } else {
      None
    }
  }

  def rpcPassword: String = config.getString("bitcoin-s.server.password")

  def exists(): Boolean = {
    baseDatadir.resolve("bitcoin-s.conf").toFile.isFile
  }

  def withOverrides(configs: Vector[Config]): BitcoinSAppConfig = {
    BitcoinSAppConfig(baseDatadir, configs ++ configOverrides)
  }
}

/** Implicit conversions that allow a unified configuration
  * to be passed in wherever a specializes one is required
  */
object BitcoinSAppConfig extends Logging {

  def fromConfig(config: Config)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    val configDataDir: Path = Paths.get(config.getString("bitcoin-s.datadir"))
    BitcoinSAppConfig(configDataDir, Vector(config))
  }

  def fromClassPathConfig()(implicit system: ActorSystem): BitcoinSAppConfig = {
    fromConfig(ConfigFactory.load())
  }

  def fromDatadir(datadir: Path, confs: Config*)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    BitcoinSAppConfig(datadir, confs.toVector)
  }

  def fromDatadirWithServerArgs(
      datadir: Path,
      serverArgsParser: ServerArgParser)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    fromDatadir(datadir, serverArgsParser.toConfig)
  }

  /** Constructs an app configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(confs: Config*)(implicit
      system: ActorSystem): BitcoinSAppConfig =
    BitcoinSAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs.toVector)

  def fromDefaultDatadirWithBundleConf(confs: Vector[Config] = Vector.empty)(
      implicit system: ActorSystem): BitcoinSAppConfig = {
    fromDatadirWithBundleConf(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs)
  }

  def fromDatadirWithBundleConf(
      datadir: Path,
      confs: Vector[Config] = Vector.empty)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    val baseConf: BitcoinSAppConfig =
      fromDatadir(datadir, confs: _*)

    // Grab saved bundle config
    val bundleConfFile =
      toChainConf(baseConf).baseDatadir.resolve("bitcoin-s-bundle.conf")
    val extraConfig = if (Files.isReadable(bundleConfFile)) {
      ConfigFactory.parseFile(bundleConfFile.toFile)
    } else {
      logger.debug("No saved bundle config found")
      ConfigFactory.empty()
    }

    baseConf.copyWithConfig(extraConfig +: confs)
  }

  /** Resolve BitcoinSAppConfig in the following order of precedence
    * 1. Server args
    * 2. bitcoin-s-bundle.conf
    * 3. bitcoin-s.conf
    * 4. application.conf
    * 5. reference.conf
    */
  def fromDatadirWithBundleConfWithServerArgs(
      datadir: Path,
      serverArgParser: ServerArgParser)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    fromDatadirWithBundleConf(datadir, Vector(serverArgParser.toConfig))
  }

  /** Creates a BitcoinSAppConfig the the given daemon args to a server */
  def fromDefaultDatadirWithServerArgs(serverArgParser: ServerArgParser)(
      implicit system: ActorSystem): BitcoinSAppConfig = {
    val config = serverArgParser.toConfig
    fromConfig(config)
  }

  /** Converts the given config to a wallet config */
  def toWalletConf(conf: BitcoinSAppConfig): WalletAppConfig = {
    conf.walletConf
  }

  /** Converts the given config to a chain config */
  def toChainConf(conf: BitcoinSAppConfig): ChainAppConfig = {
    conf.chainConf
  }

  /** Converts the given config to a node config */
  def toNodeConf(conf: BitcoinSAppConfig): NodeAppConfig = {
    conf.nodeConf
  }

  /** Converts the given config to a bitcoind rpc config */
  def toBitcoindRpcConf(conf: BitcoinSAppConfig): BitcoindRpcAppConfig = {
    conf.bitcoindRpcConf
  }
}
