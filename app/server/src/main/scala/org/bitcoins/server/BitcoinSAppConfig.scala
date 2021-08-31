package org.bitcoins.server

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import grizzled.slf4j.Logging
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.commons.file.FileUtil
import org.bitcoins.commons.util.ServerArgParser
import org.bitcoins.core.util.{FutureUtil, StartStopAsync}
import org.bitcoins.dlc.node.config.DLCNodeAppConfig
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.keymanager.config.KeyManagerAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.rpc.config.BitcoindRpcAppConfig
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.wallet.config.WalletAppConfig

import java.nio.file.{Files, Path, Paths}
import scala.concurrent.Future

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
    private val directory: Path,
    private val confs: Config*)(implicit system: ActorSystem)
    extends StartStopAsync[Unit] {
  import system.dispatcher
  lazy val walletConf: WalletAppConfig = WalletAppConfig(directory, confs: _*)
  lazy val nodeConf: NodeAppConfig = NodeAppConfig(directory, confs: _*)
  lazy val chainConf: ChainAppConfig = ChainAppConfig(directory, confs: _*)
  lazy val dlcConf: DLCAppConfig = DLCAppConfig(directory, confs: _*)
  lazy val torConf: TorAppConfig = TorAppConfig(directory, confs: _*)

  lazy val dlcNodeConf: DLCNodeAppConfig =
    DLCNodeAppConfig(directory, confs: _*)

  def copyWithConfig(newConfs: Vector[Config]): BitcoinSAppConfig = {
    val configs = newConfs ++ confs
    BitcoinSAppConfig(directory, configs: _*)
  }

  lazy val kmConf: KeyManagerAppConfig =
    KeyManagerAppConfig(directory, confs: _*)

  lazy val bitcoindRpcConf: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig(directory, confs: _*)

  /** Initializes the wallet, node and chain projects */
  override def start(): Future[Unit] = {
    val configs = List(kmConf,
                       walletConf,
                       torConf,
                       nodeConf,
                       chainConf,
                       bitcoindRpcConf,
                       dlcConf)

    FutureUtil.sequentially(configs)(_.start()).map(_ => ())
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
      AppConfig.getBaseConfig(baseDatadir = directory, confs.toList)
    val resolved = finalConfig.resolve()

    resolved.checkValid(ConfigFactory.defaultReference(), "bitcoin-s")

    resolved
  }

  def rpcPort: Int = config.getInt("bitcoin-s.server.rpcport")

  def rpcBindOpt: Option[String] = {
    if (config.hasPath("bitcoin-s.server.rpcbind")) {
      Some(config.getString("bitcoin-s.server.rpcbind"))
    } else {
      None
    }
  }

  def exists(): Boolean = {
    directory.resolve("bitcoin-s.conf").toFile.isFile
  }

  def withOverrides(configs: Config*): BitcoinSAppConfig = {
    BitcoinSAppConfig(directory, configs ++ confs: _*)
  }

  /** Zips $HOME/.bitcoin-s
    */
  def zipDatadir(target: Path): Path = {
    FileUtil.zipDirectory(
      source = directory,
      target = target,
      // we don't want to store chaindb.sqlite as these databases are huge
      // skip logs and binaries as these can be large as well
      fileNameFilter =
        Vector(".*chaindb.sqlite$".r, ".*bitcoin-s.log$".r, ".*/binaries/.*".r)
    )
  }
}

/** Implicit conversions that allow a unified configuration
  * to be passed in wherever a specializes one is required
  */
object BitcoinSAppConfig extends Logging {

  def fromConfig(config: Config)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    val configDataDir: Path = Paths.get(config.getString("bitcoin-s.datadir"))
    BitcoinSAppConfig(configDataDir, config)
  }

  def fromClassPathConfig()(implicit system: ActorSystem): BitcoinSAppConfig = {
    fromConfig(ConfigFactory.load())
  }

  def fromDatadir(datadir: Path, confs: Config*)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    BitcoinSAppConfig(datadir, confs: _*)
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
    BitcoinSAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs: _*)

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

  import scala.language.implicitConversions

  /** Converts the given implicit config to a wallet config */
  implicit def implicitToWalletConf(implicit
      conf: BitcoinSAppConfig): WalletAppConfig =
    conf.walletConf

  /** Converts the given config to a wallet config */
  implicit def toWalletConf(conf: BitcoinSAppConfig): WalletAppConfig =
    conf.walletConf

  /** Converts the given implicit config to a chain config */
  implicit def implicitToChainConf(implicit
      conf: BitcoinSAppConfig): ChainAppConfig =
    conf.chainConf

  /** Converts the given config to a chain config */
  implicit def toChainConf(conf: BitcoinSAppConfig): ChainAppConfig =
    conf.chainConf

  /** Converts the given implicit config to a node config */
  implicit def implicitToNodeConf(implicit
      conf: BitcoinSAppConfig): NodeAppConfig =
    conf.nodeConf

  /** Converts the given config to a node config */
  implicit def toNodeConf(conf: BitcoinSAppConfig): NodeAppConfig =
    conf.nodeConf

  /** Converts the given implicit config to a bitcoind rpc config */
  implicit def implicitToBitcoindRpcConf(implicit
      conf: BitcoinSAppConfig): BitcoindRpcAppConfig =
    conf.bitcoindRpcConf

  /** Converts the given config to a bitcoind rpc config */
  implicit def toBitcoindRpcConf(
      conf: BitcoinSAppConfig): BitcoindRpcAppConfig =
    conf.bitcoindRpcConf

}
