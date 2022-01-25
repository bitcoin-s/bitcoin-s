package org.bitcoins.dlc.node.config

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.commons.config.{AppConfig, AppConfigFactory}
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.util.{FutureUtil, NetworkUtil}
import org.bitcoins.dlc.node.DLCNode
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.net.{InetSocketAddress, URI}
import java.nio.file.Path
import scala.concurrent._

/** Configuration for the Bitcoin-S wallet
  *
  * @param directory The data directory of the wallet
  * @param conf      Optional sequence of configuration overrides
  */
case class DLCNodeAppConfig(baseDatadir: Path, configOverrides: Vector[Config])(
    implicit ec: ExecutionContext)
    extends AppConfig {

  override protected[bitcoins] def moduleName: String =
    DLCNodeAppConfig.moduleName

  override protected[bitcoins] type ConfigType = DLCNodeAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): DLCNodeAppConfig =
    DLCNodeAppConfig(baseDatadir, configs.toVector)

  override def start(): Future[Unit] = {
    FutureUtil.unit
  }

  override def stop(): Future[Unit] = Future.unit

  lazy val torConf: TorAppConfig =
    TorAppConfig(baseDatadir, Some(moduleName), configOverrides)

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] =
    torConf.socks5ProxyParams

  lazy val torParams: Option[TorParams] = torConf.torParams

  lazy val listenAddress: InetSocketAddress = {
    val str = config.getString(s"bitcoin-s.$moduleName.listen")
    val uri = new URI("tcp://" + str)
    new InetSocketAddress(uri.getHost, uri.getPort)
  }

  lazy val externalIP: Option[InetSocketAddress] = {
    val path = s"bitcoin-s.$moduleName.external-ip"
    if (config.hasPath(path)) {
      val str = config.getString(path)
      Some(NetworkUtil.parseInetSocketAddress(str, listenAddress.getPort))
    } else {
      None
    }
  }

  def createDLCNode(dlcWallet: DLCWalletApi)(implicit
      system: ActorSystem): DLCNode = {
    DLCNode(dlcWallet)(system, this)
  }
}

object DLCNodeAppConfig extends AppConfigFactory[DLCNodeAppConfig] {
  override val moduleName: String = "dlcnode"

  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): DLCNodeAppConfig =
    DLCNodeAppConfig(datadir, confs)
}
