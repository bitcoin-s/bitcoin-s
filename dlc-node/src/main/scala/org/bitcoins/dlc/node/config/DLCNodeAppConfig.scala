package org.bitcoins.dlc.node.config

import com.typesafe.config.Config
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db._
import org.bitcoins.tor.Socks5ProxyParams

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
    private val conf: Config*)
    extends AppConfig {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList

  override protected[bitcoins] def moduleName: String = "dlcnode"

  override protected[bitcoins] type ConfigType = DLCNodeAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): DLCNodeAppConfig =
    DLCNodeAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def start(): Future[Unit] = {
    FutureUtil.unit
  }

  override def stop(): Future[Unit] = Future.unit

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

  lazy val listenAddress: InetSocketAddress = {
    val str = config.getString(s"bitcoin-s.$moduleName.listen")
    val uri = new URI(str)
    InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
  }
}
