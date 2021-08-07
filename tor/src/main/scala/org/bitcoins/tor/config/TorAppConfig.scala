package org.bitcoins.tor.config

import com.typesafe.config.Config
import org.bitcoins.commons.config.{AppConfig, AppConfigFactory, ConfigOps}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.tor.TorProtocolHandler.{Password, SafeCookie}
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}

import java.io.File
import java.nio.file.Path
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S node
  * @param directory The data directory of the node
  * @param confs Optional sequence of configuration overrides
  */
case class TorAppConfig(private val directory: Path, private val confs: Config*)
    extends AppConfig {
  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] def moduleName: String = TorAppConfig.moduleName
  override protected[bitcoins] type ConfigType = TorAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): TorAppConfig =
    TorAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  /** Ensures correct tables and other required information is in
    * place for our node.
    */
  override def start(): Future[Unit] = Future.unit

  override def stop(): Future[Unit] = Future.unit

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] = {
    if (config.getBoolean("bitcoin-s.proxy.enabled")) {
      Some(
        Socks5ProxyParams(
          address = NetworkUtil.parseInetSocketAddress(
            config.getString("bitcoin-s.proxy.socks5"),
            Socks5ProxyParams.DefaultPort),
          credentialsOpt = None,
          randomizeCredentials = true
        )
      )
    } else {
      None
    }
  }

  lazy val torParams: Option[TorParams] = {
    if (config.getBoolean("bitcoin-s.tor.enabled")) {
      val control = NetworkUtil.parseInetSocketAddress(
        config.getString("bitcoin-s.tor.control"),
        TorParams.DefaultControlPort)

      val auth = config.getStringOrNone("bitcoin-s.tor.password") match {
        case Some(pass) => Password(pass)
        case None       => SafeCookie()
      }

      val privKeyPath =
        config.getStringOrNone("bitcoin-s.tor.privateKeyPath") match {
          case Some(path) => new File(path).toPath
          case None       => datadir.resolve("tor_priv_key")
        }

      Some(TorParams(control, auth, privKeyPath))
    } else {
      None
    }
  }
}

object TorAppConfig extends AppConfigFactory[TorAppConfig] {

  override val moduleName: String = "tor"

  /** Constructs a tor configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): TorAppConfig =
    TorAppConfig(datadir, confs: _*)
}
