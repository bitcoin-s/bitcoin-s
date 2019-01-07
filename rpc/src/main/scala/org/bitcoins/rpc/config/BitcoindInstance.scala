package org.bitcoins.rpc.config

import java.io.File
import java.net.URI
import java.nio.file.Paths

import com.typesafe.config._
import org.bitcoins.core.config._

import scala.util.{Failure, Properties, Success, Try}

/**
  * Created by chris on 4/29/17.
  */
sealed trait BitcoindInstance {
  require(
    rpcUri.getPort == rpcPort,
    s"RpcUri and the rpcPort in authCredentials are different ${rpcUri} authcred: ${rpcPort}")
  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: BitcoindAuthCredentials
  def zmqConfig: ZmqConfig

  def rpcPort: Int = authCredentials.rpcPort
}

object BitcoindInstance {
  private case class BitcoindInstanceImpl(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig = ZmqConfig()
  ) extends BitcoindInstance

  def apply(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqConfig: ZmqConfig = ZmqConfig()
  ): BitcoindInstance = {
    BitcoindInstanceImpl(network,
                         uri,
                         rpcUri,
                         authCredentials,
                         zmqConfig = zmqConfig)
  }

  private val DEFAULT_DATADIR =
    Paths.get(Properties.userHome, ".bitcoin")

  private val DEFAULT_CONF_FILE = DEFAULT_DATADIR.resolve("bitcoin.conf")

  def fromDatadir(datadir: File = DEFAULT_DATADIR.toFile): BitcoindInstance = {
    require(datadir.exists, s"${datadir.getPath} does not exist!")
    require(datadir.isDirectory, s"${datadir.getPath} is not a directory!")

    val file = Paths.get(datadir.getAbsolutePath, "bitcoin.conf").toFile
    fromConfigFile(file)
  }

  def fromConfigFile(
      file: File = DEFAULT_CONF_FILE.toFile): BitcoindInstance = {
    require(file.exists, s"${file.getPath} does not exist!")
    require(file.isFile, s"${file.getPath} is not a file!")

    val config = ConfigFactory.parseFile(
      file,
      ConfigParseOptions.defaults
        .setSyntax(ConfigSyntax.PROPERTIES)) // bitcoin.conf is not a proper .conf file, uses Java properties=like syntax

    val configWithDatadir =
      if (config.hasPath("datadir")) {
        config
      } else {
        config.withValue("datadir",
                         ConfigValueFactory.fromAnyRef(file.getParent))
      }

    fromConfig(configWithDatadir)
  }

  def fromConfig(config: Config): BitcoindInstance = {
    val datadirStr = Try(config.getString("datadir"))
      .getOrElse(
        throw new IllegalArgumentException(
          "Provided config does not contain \"datadir\" setting!"))

    val datadir = new File(datadirStr)
    require(datadir.exists, s"Datadir $datadirStr does not exist!")
    require(datadir.isDirectory, s"Datadir $datadirStr is not directory")
    fromConfig(config, datadir)
  }

  def fromConfig(
      config: Config,
      datadir: File
  ): BitcoindInstance = {
    val network = getNetwork(config)

    val uri = getUri(config, network)
    val rpcUri = getRpcUri(config, network)

    val username = config.getString("rpcuser")
    val password = config.getString("rpcpassword")
    val authCredentials =
      BitcoindAuthCredentials(username = username,
                              password = password,
                              rpcPort = rpcUri.getPort,
                              datadir = datadir)

    BitcoindInstance(network,
                     uri,
                     rpcUri,
                     authCredentials,
                     zmqConfig = ZmqConfig.fromConfig(config))
  }

  private def isSet(config: Config, path: String): Boolean = {
    Try(config.getInt(path))
      .map(_ == 1)
      .getOrElse(false)
  }

  private def getNetwork(config: Config): BitcoinNetwork = {
    val isTestnet = isSet(config, path = "testnet")
    val isRegTest = isSet(config, path = "regtest")

    (isRegTest, isTestnet) match {
      case (true, true) =>
        throw new IllegalArgumentException(
          """"Cannot set both "regtest" and "testnet" options""")
      case (true, false)  => RegTest
      case (false, true)  => TestNet3
      case (false, false) => MainNet
    }
  }

  private def getUri(config: Config, network: NetworkParameters): URI = {
    val port = Try(config.getInt("port")).getOrElse(network.port)
    val host = Try(config.getString("bind")).getOrElse("localhost")
    val uriT = Try {
      new URI(s"http://$host:$port")
    }

    uriT match {
      case Success(uriSuccess) => uriSuccess
      case Failure(exception) =>
        throw new IllegalArgumentException(
          s"Could not construct URI from host $host and port $port",
          exception)
    }

  }

  private def getRpcUri(config: Config, network: NetworkParameters): URI = {
    val rpcPort = Try(config.getInt("rpcport")).getOrElse(network.rpcPort)
    val rpcHost = Try(config.getString("rpcbind")).getOrElse("localhost")

    val rpcUriT = Try {
      new URI(s"http://$rpcHost:$rpcPort")
    }
    rpcUriT match {
      case Success(uriSuccess) => uriSuccess
      case Failure(exception) =>
        throw new IllegalArgumentException(
          s"Could not construct URI from host $rpcHost and port $rpcPort",
          exception)
    }
  }

}
