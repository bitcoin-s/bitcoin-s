package org.bitcoins.server

import java.io.File
import java.net.URI
import java.nio.file._

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db._
import org.bitcoins.node.NodeType
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.config._

import scala.concurrent.{ExecutionContext, Future}

/** Configuration for a BitcoindRpcClient
  * @param directory The data directory of the Bitcoin-S instance
  * @param confs Optional sequence of configuration overrides
  */
case class BitcoindRpcAppConfig(
    private val directory: Path,
    private val confs: Config*)(implicit val ec: ExecutionContext)
    extends AppConfig {
  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] def moduleName: String = "bitcoind"
  override protected[bitcoins] type ConfigType = BitcoindRpcAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): BitcoindRpcAppConfig =
    BitcoindRpcAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  lazy val nodeConf: NodeAppConfig = NodeAppConfig(directory, confs: _*)

  override def start(): Future[Unit] = {
    nodeConf.nodeType match {
      case NodeType.BitcoindBackend =>
        client.start().map(_ => ())
      case NodeType.SpvNode | NodeType.NeutrinoNode | NodeType.FullNode =>
        FutureUtil.unit
    }
  }

  override def stop(): Future[Unit] = FutureUtil.unit

  lazy val DEFAULT_BINARY_PATH: File =
    BitcoindInstance.DEFAULT_BITCOIND_LOCATION

  lazy val binary: File =
    new File(
      config.getStringOrElse("bitcoin-s.bitcoind-rpc.binary",
                             DEFAULT_BINARY_PATH.toString))

  lazy val bitcoindDataDir = new File(
    config.getStringOrElse("bitcoin-s.bitcoind-rpc.datadir",
                           BitcoindConfig.DEFAULT_DATADIR.toString))

  lazy val bind = new URI({
    val baseUrl =
      config.getStringOrElse("bitcoin-s.bitcoind-rpc.bind", "localhost")
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })

  lazy val port: Int =
    config.getIntOrElse("bitcoin-s.bitcoind-rpc.port", network.port)

  lazy val uri: URI = new URI(s"$bind:$port")

  lazy val rpcBind = new URI({
    val baseUrl =
      config.getStringOrElse("bitcoin-s.bitcoind-rpc.rpcbind", "localhost")
    if (baseUrl.startsWith("http")) baseUrl
    else "http://" + baseUrl
  })

  lazy val rpcPort: Int =
    config.getIntOrElse("bitcoin-s.bitcoind-rpc.rpcport", network.rpcPort)

  lazy val rpcUri: URI = new URI(s"$rpcBind:$rpcPort")

  lazy val rpcUser: String = config.getString("bitcoin-s.bitcoind-rpc.rpcuser")

  lazy val rpcPassword: String =
    config.getString("bitcoin-s.bitcoind-rpc.rpcpassword")

  lazy val authCredentials: BitcoindAuthCredentials =
    BitcoindAuthCredentials.PasswordBased(rpcUser, rpcPassword)

  lazy val zmqPortOpt: Option[Int] =
    config.getIntOpt("bitcoin-s.bitcoind-rpc.zmqport")

  lazy val zmqConfig: ZmqConfig = zmqPortOpt match {
    case Some(zmqPort) => ZmqConfig.fromPort(zmqPort)
    case None          => ZmqConfig()
  }

  lazy val bitcoindInstance: BitcoindInstance =
    BitcoindInstance(network = network,
                     uri = uri,
                     rpcUri = rpcUri,
                     authCredentials = authCredentials,
                     zmqConfig = zmqConfig,
                     binary = binary,
                     datadir = bitcoindDataDir)

  lazy val client: BitcoindRpcClient = {
    val version = bitcoindInstance.getVersion
    implicit val system: ActorSystem =
      ActorSystem.create("bitcoind-rpc-client-created-by-bitcoin-s", config)
    BitcoindRpcClient.fromVersion(version, bitcoindInstance)
  }

}

object BitcoindRpcAppConfig extends AppConfigFactory[BitcoindRpcAppConfig] {

  /** Constructs a node configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): BitcoindRpcAppConfig =
    BitcoindRpcAppConfig(datadir, confs: _*)
}
