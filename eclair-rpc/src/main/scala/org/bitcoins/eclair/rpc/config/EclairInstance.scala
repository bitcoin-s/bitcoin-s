package org.bitcoins.eclair.rpc.config

import java.io.File
import java.net.URI
import java.nio.file.Paths

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.ln.LnPolicy

import scala.util.Properties

sealed trait EclairInstance {
  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: EclairAuthCredentials
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
      authCredentials: EclairAuthCredentials)
      extends EclairInstance

  def apply(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: EclairAuthCredentials): EclairInstance = {
    EclairInstanceImpl(network, uri, rpcUri, authCredentials)
  }

  private val DEFAULT_DATADIR = Paths.get(Properties.userHome, ".eclair")

  private val DEFAULT_CONF_FILE = DEFAULT_DATADIR.resolve("eclair.conf")

  def fromDatadir(datadir: File = DEFAULT_DATADIR.toFile): EclairInstance = {
    require(datadir.exists, s"${datadir.getPath} does not exist!")
    require(datadir.isDirectory, s"${datadir.getPath} is not a directory!")

    val eclairConf = new File(datadir.getAbsolutePath + "/eclair.conf")

    fromConfigFile(eclairConf)

  }

  def fromConfigFile(file: File = DEFAULT_CONF_FILE.toFile): EclairInstance = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val config = ConfigFactory.parseFile(file)

    fromConfig(config, file.getParentFile)
  }

  /**
    * $fromConfigDoc
    */
  def fromConfig(config: Config, datadir: File): EclairInstance = {
    fromConfig(config, Some(datadir))
  }

  /**
    * $fromConfigDoc
    */
  def fromConfig(config: Config): EclairInstance = {
    fromConfig(config, None)
  }

  private def fromConfig(
      config: Config,
      datadir: Option[File]): EclairInstance = {
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

    val instance = EclairInstance(network = np,
                                  uri = uri,
                                  rpcUri = rpcUri,
                                  authCredentials = eclairAuth)

    instance
  }
}
