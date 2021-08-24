package org.bitcoins.dlc.node.config

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.commons.config.{AppConfig, AppConfigFactory}
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.util.FutureUtil
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
case class DLCNodeAppConfig(
    private val directory: Path,
    private val conf: Config*)(implicit ec: ExecutionContext)
    extends AppConfig {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList

  override protected[bitcoins] def moduleName: String =
    DLCNodeAppConfig.moduleName

  override protected[bitcoins] type ConfigType = DLCNodeAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): DLCNodeAppConfig =
    DLCNodeAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def start(): Future[Unit] = {
    FutureUtil.unit
  }

  override def stop(): Future[Unit] = Future.unit

  lazy val torConf: TorAppConfig =
    TorAppConfig(directory, conf: _*)

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] =
    torConf.socks5ProxyParams

  lazy val torParams: Option[TorParams] = torConf.torParams

  lazy val listenAddress: InetSocketAddress = {
    val str = config.getString(s"bitcoin-s.$moduleName.listen")
    val uri = new URI("tcp://" + str)
    new InetSocketAddress(uri.getHost, uri.getPort)
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
    DLCNodeAppConfig(datadir, confs: _*)
}
