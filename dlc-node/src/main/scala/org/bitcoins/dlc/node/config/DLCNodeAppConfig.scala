package org.bitcoins.dlc.node.config

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db._
import org.bitcoins.dlc.node.DLCNode
import org.bitcoins.tor
import org.bitcoins.tor.{Socks5ProxyParams, TorParams}
import org.bitcoins.tor.TorProtocolHandler.{Password, SafeCookie}

import java.io.File
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
      val uri = new URI("tcp://" + config.getString("bitcoin-s.proxy.socks5"))
      val sock5 = InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
      Some(
        Socks5ProxyParams(
          address = sock5,
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
      val controlURI = new URI(config.getString("bitcoin-s.tor.control"))
      val control = InetSocketAddress.createUnresolved(controlURI.getHost,
                                                       controlURI.getPort)

      val auth = config.getStringOrNone("bitcoin-s.tor.password") match {
        case Some(pass) => Password(pass)
        case None       => SafeCookie() // todo allow configuring the cookie?
      }

      val privKeyPath =
        config.getStringOrNone("bitcoin-s.tor.privateKeyPath") match {
          case Some(path) => new File(path).toPath
          case None       => datadir.resolve("tor_priv_key")
        }

      Some(tor.TorParams(control, auth, privKeyPath))
    } else {
      None
    }
  }

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
