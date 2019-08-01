package org.bitcoins.server

import com.typesafe.config.Config
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.chain.config.ChainAppConfig
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import java.nio.file.Path
import org.bitcoins.db.AppConfig

/**
  * A unified config class for all submodules of Bitcoin-S
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
    private val confs: Config*) {
  val walletConf = WalletAppConfig(directory, confs: _*)
  val nodeConf = NodeAppConfig(directory, confs: _*)
  val chainConf = ChainAppConfig(directory, confs: _*)

  /** Initializes the wallet, node and chain projects */
  def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    val futures = List(walletConf.initialize(),
                       nodeConf.initialize(),
                       chainConf.initialize())

    Future.sequence(futures).map(_ => ())
  }

  /** The underlying config the result of our fields derive from */
  lazy val config: Config = {
    assert(chainConf.config == nodeConf.config)
    assert(nodeConf.config == walletConf.config)

    // there's nothing special about nodeConf, they should all
    // be equal
    nodeConf.config
  }
}

/**
  * Implicit conversions that allow a unified configuration
  * to be passed in wherever a specializes one is required
  */
object BitcoinSAppConfig {

  /** Constructs an app configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(confs: Config*): BitcoinSAppConfig =
    BitcoinSAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs: _*)

  import scala.language.implicitConversions

  /** Converts the given implicit config to a wallet config */
  implicit def implicitToWalletConf(
      implicit conf: BitcoinSAppConfig): WalletAppConfig =
    conf.walletConf

  /** Converts the given config to a wallet config */
  implicit def toWalletConf(conf: BitcoinSAppConfig): WalletAppConfig =
    conf.walletConf

  /** Converts the given implicit config to a chain config */
  implicit def implicitToChainConf(
      implicit conf: BitcoinSAppConfig): ChainAppConfig =
    conf.chainConf

  /** Converts the given config to a chain config */
  implicit def toChainConf(conf: BitcoinSAppConfig): ChainAppConfig =
    conf.chainConf

  /** Converts the given implicit config to a node config */
  implicit def implicitToNodeConf(
      implicit conf: BitcoinSAppConfig): NodeAppConfig =
    conf.nodeConf

  /** Converts the given config to a node config */
  implicit def toNodeConf(conf: BitcoinSAppConfig): NodeAppConfig =
    conf.nodeConf

}
