package org.bitcoins.server

import com.typesafe.config.Config
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.chain.config.ChainAppConfig
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/**
  * A unified config class for all submodules of Bitcoin-S
  * that accepts configuration. Thanks to implicit definitions
  * in this case class' companion object an instance
  * of this class can be passed in anywhere a wallet,
  * chain or node config is required.
  */
case class BitcoinSAppConfig(private val confs: Config*) {
  val walletConf = WalletAppConfig(confs: _*)
  val nodeConf = NodeAppConfig(confs: _*)
  val chainConf = ChainAppConfig(confs: _*)

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
