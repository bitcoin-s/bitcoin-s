package org.bitcoins.eclair.rpc.config

import java.io.File
import java.net.{InetSocketAddress, URI}
import java.nio.file.Paths

import com.sun.jndi.toolkit.url.Uri
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.ln.LnPolicy
import org.bitcoins.rpc.config.{BitcoindAuthCredentials, ZmqConfig}

import scala.util.Properties

sealed trait EclairInstance {
  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: EclairAuthCredentials
  def logbackXmlPath: Option[String]
  def bitcoindRpcUri: Option[URI]
  def bitcoindAuthCredentials: Option[BitcoindAuthCredentials]
  def zmqConfig: Option[ZmqConfig]
}

/**
  * @define fromConfigDoc
  * Parses a [[com.typesafe.config.Config Config]] in the format of this
  * [[https://github.com/ACINQ/eclair/blob/master/eclair-core/src/main/resources/reference.conf sample reference.conf]]
  * file to a
  * [[org.bitcoins.eclair.rpc.config.EclairInstance EclairInstance]]
  */
object EclairInstance {
  private case class EclairInstanceImpl(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: EclairAuthCredentials,
      logbackXmlPath: Option[String],
      bitcoindRpcUri: Option[URI],
      bitcoindAuthCredentials: Option[BitcoindAuthCredentials],
      zmqConfig: Option[ZmqConfig])
      extends EclairInstance

  def apply(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: EclairAuthCredentials,
      logbackXmlPath: Option[String],
      bitcoindRpcUri: Option[URI] = None,
      bitcoindAuthCredentials: Option[BitcoindAuthCredentials] = None,
      zmqConfig: Option[ZmqConfig] = None): EclairInstance = {
    EclairInstanceImpl(network,
                       uri,
                       rpcUri,
                       authCredentials,
                       logbackXmlPath,
                       bitcoindRpcUri,
                       bitcoindAuthCredentials,
                       zmqConfig)
  }

  private val DEFAULT_DATADIR = Paths.get(Properties.userHome, ".eclair")

  private val DEFAULT_CONF_FILE = DEFAULT_DATADIR.resolve("eclair.conf")

  private def toInetSocketAddress(string: String): InetSocketAddress = {
    val uri = new Uri(string)
    new InetSocketAddress(uri.getHost, uri.getPort)
  }

  def fromDatadir(
      datadir: File = DEFAULT_DATADIR.toFile,
      logbackXml: Option[String]): EclairInstance = {
    require(datadir.exists, s"${datadir.getPath} does not exist!")
    require(datadir.isDirectory, s"${datadir.getPath} is not a directory!")

    val eclairConf = new File(datadir.getAbsolutePath + "/eclair.conf")

    fromConfigFile(eclairConf, logbackXml)

  }

  def fromConfigFile(
      file: File = DEFAULT_CONF_FILE.toFile,
      logbackXml: Option[String]): EclairInstance = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val config = ConfigFactory.parseFile(file)

    fromConfig(config, file.getParentFile, logbackXml)
  }

  /**
    * $fromConfigDoc
    */
  def fromConfig(
      config: Config,
      datadir: File,
      logbackXml: Option[String]): EclairInstance = {
    fromConfig(config, Some(datadir), logbackXml)
  }

  /**
    * $fromConfigDoc
    */
  def fromConfig(config: Config): EclairInstance = {
    fromConfig(config, None, None)
  }

  private def fromConfig(
      config: Config,
      datadir: Option[File],
      logbackXml: Option[String]): EclairInstance = {
    val chain = ConfigUtil.getStringOrElse(config, "eclair.chain", "testnet")

    //  default conf: https://github.com/ACINQ/eclair/blob/master/eclair-core/src/main/resources/reference.conf
    val serverBindingIp =
      ConfigUtil.getStringOrElse(config, "eclair.server.binding-ip", "0.0.0.0")

    val serverPort = ConfigUtil.getIntOrElse(config,
                                             "eclair.server.port",
                                             LnPolicy.DEFAULT_LN_P2P_PORT)

    //  default conf: https://github.com/ACINQ/eclair/blob/master/eclair-core/src/main/resources/reference.conf
    val rpcHost =
      ConfigUtil.getStringOrElse(config, "eclair.api.binding-ip", "127.0.0.1")

    val rpcPort = ConfigUtil.getIntOrElse(config,
                                          "eclair.api.port",
                                          LnPolicy.DEFAULT_ECLAIR_API_PORT)

    val np: NetworkParameters = chain match {
      case "regtest" => RegTest
      case "testnet" => TestNet3
      case "mainnet" => MainNet
      case network: String =>
        throw new IllegalArgumentException(
          s"Unknown network $network in eclair.conf")
    }

    val uri: URI = new URI(s"http://$serverBindingIp:$serverPort")

    val rpcUri: URI = new URI(s"http://$rpcHost:$rpcPort")

    val eclairAuth = EclairAuthCredentials.fromConfig(config, datadir)

    val bitcoindRpcHost =
      ConfigUtil.getStringOrElse(config, "eclair.bitcoind.host", "127.0.0.1")
    val bitcoindRpcPort =
      ConfigUtil.getIntOrElse(config, "eclair.bitcoind.rpcport", np.rpcPort)
    val bitcoindRpcUri = new URI(s"http://$bitcoindRpcHost:$bitcoindRpcPort")

    val bitcoindRpcUser =
      ConfigUtil.getStringOrElse(config, "eclair.bitcoind.rpcuser", "foo")
    val bitcoindRpcPass =
      ConfigUtil.getStringOrElse(config, "eclair.bitcoind.rpcpassword", "bar")
    val bitcoindAuthCredentials =
      BitcoindAuthCredentials.PasswordBased(bitcoindRpcUser, bitcoindRpcPass)

    val rawBlock: Option[InetSocketAddress] =
      ConfigUtil
        .getString(config, "eclair.bitcoind.zmqblock")
        .map(toInetSocketAddress)
    val rawTx: Option[InetSocketAddress] =
      ConfigUtil
        .getString(config, "eclair.bitcoind.zmqtx")
        .map(toInetSocketAddress)

    val zmqConfig = ZmqConfig(rawBlock = rawBlock, rawTx = rawTx)

    EclairInstance(
      network = np,
      uri = uri,
      rpcUri = rpcUri,
      authCredentials = eclairAuth,
      logbackXmlPath = logbackXml,
      bitcoindRpcUri = Some(bitcoindRpcUri),
      bitcoindAuthCredentials = Some(bitcoindAuthCredentials),
      zmqConfig = Some(zmqConfig)
    )
  }
}
