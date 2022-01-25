package org.bitcoins.rpc.config

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.commons.config.{AppConfig, ConfigOps}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.util.AppConfigFactoryActorSystem
import org.bitcoins.tor.Socks5ProxyParams
import org.bitcoins.tor.config.TorAppConfig

import java.io.File
import java.net.{InetSocketAddress, URI}
import java.nio.file._
import scala.concurrent.Future

/** Configuration for a BitcoindRpcClient
  * @param directory The data directory of the Bitcoin-S instance
  * @param confs Optional sequence of configuration overrides
  */
case class BitcoindRpcAppConfig(
    baseDatadir: Path,
    configOverrides: Vector[Config])(implicit val system: ActorSystem)
    extends AppConfig {

  import system.dispatcher

  override protected[bitcoins] def moduleName: String =
    BitcoindRpcAppConfig.moduleName

  override protected[bitcoins] type ConfigType = BitcoindRpcAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): BitcoindRpcAppConfig =
    BitcoindRpcAppConfig(baseDatadir, configs.toVector)

  override def start(): Future[Unit] = Future.unit

  override def stop(): Future[Unit] = Future.unit

  lazy val DEFAULT_BINARY_PATH: Option[File] =
    BitcoindInstanceLocal.DEFAULT_BITCOIND_LOCATION

  lazy val binaryOpt: Option[File] =
    config.getStringOrNone("bitcoin-s.bitcoind-rpc.binary").map(new File(_))

  lazy val bitcoindDataDir = new File(
    config.getStringOrElse("bitcoin-s.bitcoind-rpc.datadir",
                           BitcoindConfig.DEFAULT_DATADIR.toString))

  lazy val host = new URI({
    val baseUrl = {
      config.getStringOrNone("bitcoin-s.bitcoind-rpc.connect") match {
        case Some(rpcconnect) => rpcconnect
        case None =>
          config.getStringOrNone("bitcoin-s.bitcoind-rpc.bind") match {
            case Some(rpcbind) =>
              logger.warn(
                "Config option bitcoin-s.bitcoind-rpc.bind will soon be deprecated. Use bitcoin-s.bitcoind-rpc.connect instead.")
              rpcbind
            case None => "localhost"
          }
      }
    }
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })

  lazy val port: Int =
    config.getIntOrElse("bitcoin-s.bitcoind-rpc.port", network.port)

  lazy val uri: URI = new URI(s"$host:$port")

  lazy val rpcHost = new URI({
    val baseUrl = {
      config.getStringOrNone("bitcoin-s.bitcoind-rpc.rpcconnect") match {
        case Some(rpcconnect) => rpcconnect
        case None =>
          config.getStringOrNone("bitcoin-s.bitcoind-rpc.rpcbind") match {
            case Some(rpcbind) =>
              logger.warn(
                "Config option bitcoin-s.bitcoind-rpc.rpcbind will soon be deprecated. Use bitcoin-s.bitcoind-rpc.rpcconnect instead.")
              rpcbind
            case None => "localhost"
          }
      }
    }
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })

  lazy val rpcPort: Int =
    config.getIntOrElse("bitcoin-s.bitcoind-rpc.rpcport", network.rpcPort)

  lazy val rpcUri: URI = {
    val u = new URI(s"$rpcHost:$rpcPort")
    u
  }

  lazy val rpcUser: Option[String] =
    config.getStringOrNone("bitcoin-s.bitcoind-rpc.rpcuser")

  lazy val rpcPassword: Option[String] =
    config.getStringOrNone("bitcoin-s.bitcoind-rpc.rpcpassword")

  lazy val torConf: TorAppConfig =
    TorAppConfig(baseDatadir, Some(moduleName), configOverrides)

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] =
    torConf.socks5ProxyParams

  lazy val versionOpt: Option[BitcoindVersion] =
    config
      .getStringOrNone("bitcoin-s.bitcoind-rpc.version")
      .map(BitcoindVersion.fromString)

  lazy val isRemote: Boolean =
    config.getBooleanOrElse("bitcoin-s.bitcoind-rpc.remote", default = false)

  lazy val authCredentials: BitcoindAuthCredentials = rpcUser match {
    case Some(rpcUser) => {
      rpcPassword match {
        case Some(rpcPassword) =>
          BitcoindAuthCredentials.PasswordBased(rpcUser, rpcPassword)
        case None =>
          BitcoindAuthCredentials.CookieBased(network)
      }
    }
    case None => BitcoindAuthCredentials.CookieBased(network)
  }

  lazy val zmqRawBlock: Option[InetSocketAddress] =
    config.getStringOrNone("bitcoin-s.bitcoind-rpc.zmqpubrawblock").map { str =>
      val uri = URI.create(str)
      new InetSocketAddress(uri.getHost, uri.getPort)
    }

  lazy val zmqRawTx: Option[InetSocketAddress] =
    config.getStringOrNone("bitcoin-s.bitcoind-rpc.zmqpubrawtx").map { str =>
      val uri = URI.create(str)
      new InetSocketAddress(uri.getHost, uri.getPort)
    }

  lazy val zmqHashBlock: Option[InetSocketAddress] =
    config.getStringOrNone("bitcoin-s.bitcoind-rpc.zmqpubashblock").map { str =>
      val uri = URI.create(str)
      new InetSocketAddress(uri.getHost, uri.getPort)
    }

  lazy val zmqHashTx: Option[InetSocketAddress] =
    config.getStringOrNone("bitcoin-s.bitcoind-rpc.zmqpubashtx").map { str =>
      val uri = URI.create(str)
      new InetSocketAddress(uri.getHost, uri.getPort)
    }

  lazy val zmqConfig: ZmqConfig =
    ZmqConfig(zmqHashBlock, zmqRawBlock, zmqHashTx, zmqRawTx)

  lazy val bitcoindInstance = binaryOpt match {
    case Some(file) =>
      BitcoindInstanceLocal(
        network = network,
        uri = uri,
        rpcUri = rpcUri,
        authCredentials = authCredentials,
        zmqConfig = zmqConfig,
        binary = file,
        datadir = bitcoindDataDir
      )

    case None =>
      BitcoindInstanceRemote(network = network,
                             uri = uri,
                             rpcUri = rpcUri,
                             authCredentials = authCredentials,
                             zmqConfig = zmqConfig,
                             proxyParams = socks5ProxyParams)
  }

  /** Creates a bitcoind rpc client based on the [[bitcoindInstance]] configured */
  lazy val clientF: Future[BitcoindRpcClient] = {
    bitcoindInstance match {
      case local: BitcoindInstanceLocal =>
        val version = versionOpt.getOrElse(local.getVersion)
        val client = BitcoindRpcClient.fromVersion(version, bitcoindInstance)
        Future.successful(client)
      case _: BitcoindInstanceRemote =>
        //first get a generic rpc client so we can retrieve
        //the proper version of the remote running bitcoind
        val noVersionRpc = new BitcoindRpcClient(bitcoindInstance)
        val versionF = noVersionRpc.version

        //if we don't retrieve the proper version, we can
        //end up with exceptions on an rpc client that actually supports
        //specific features that are not supported across all versions of bitcoind
        //such as blockfilters
        //see: https://github.com/bitcoin-s/bitcoin-s/issues/3695#issuecomment-929492945
        versionF.map { version =>
          BitcoindRpcClient.fromVersion(version, instance = bitcoindInstance)
        }
    }
  }
}

object BitcoindRpcAppConfig
    extends AppConfigFactoryActorSystem[BitcoindRpcAppConfig] {

  override val moduleName: String = "bitcoind"

  /** Constructs a node configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */

  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      system: ActorSystem): BitcoindRpcAppConfig =
    BitcoindRpcAppConfig(datadir, confs)

}
