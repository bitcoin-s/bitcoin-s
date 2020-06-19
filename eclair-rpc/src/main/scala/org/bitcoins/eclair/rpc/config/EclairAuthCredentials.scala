package org.bitcoins.eclair.rpc.config

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.rpc.config.BitcoindAuthCredentials
import java.net.URI

sealed trait EclairAuthCredentials {

  /** The directory where our `eclair.conf` file is located */
  def datadir: Option[File]

  def bitcoinAuthOpt: Option[BitcoindAuthCredentials]

  /** `rpcusername` field in our `bitcoin.conf` file */
  def bitcoinUsername: Option[String] = {
    bitcoinAuthOpt.map(_.username)
  }

  /** `rpcpassword` field in our `bitcoin.conf` file */
  def bitcoinPassword: Option[String] = {
    bitcoinAuthOpt.map(_.password)
  }

  /** `rpcport` field in our `bitcoin.conf` file */
  def bitcoindRpcUri: URI

  /** `eclair.api.password` field in our `eclair.conf` file */
  def password: String

  /** The port for eclair's rpc client */
  def rpcPort: Int

  def copyWithDatadir(datadir: File): EclairAuthCredentials = {
    EclairAuthCredentials(password = password,
                          bitcoinAuthOpt = bitcoinAuthOpt,
                          rpcPort = rpcPort,
                          bitcoindRpcUri = bitcoindRpcUri,
                          datadir = Some(datadir))
  }
}

/**
  * @define fromConfigDoc
  * Parses a [[com.typesafe.config.Config Config]] in the format of this
  * [[https://github.com/ACINQ/eclair/blob/master/eclair-core/src/main/resources/reference.conf sample reference.conf]]
  * file to a
  * [[org.bitcoins.eclair.rpc.config.EclairAuthCredentials EclairAuthCredentials]]
  */
object EclairAuthCredentials {

  private case class AuthCredentialsImpl(
      password: String,
      bitcoinAuthOpt: Option[BitcoindAuthCredentials],
      rpcPort: Int,
      bitcoindRpcUri: URI,
      datadir: Option[File])
      extends EclairAuthCredentials

  def apply(
      password: String,
      bitcoinAuthOpt: Option[BitcoindAuthCredentials],
      rpcPort: Int,
      bitcoindRpcUri: URI,
      datadir: Option[File] = None): EclairAuthCredentials = {
    AuthCredentialsImpl(password,
                        bitcoinAuthOpt,
                        rpcPort,
                        bitcoindRpcUri,
                        datadir)
  }

  def fromDatadir(datadir: File): EclairAuthCredentials = {
    val confFile = new File(datadir.getAbsolutePath + "/eclair.conf")
    val config = ConfigFactory.parseFile(confFile)
    val auth = fromConfig(config)
    auth.copyWithDatadir(datadir = datadir)
  }

  /**
    * $fromConfigDoc
    */
  def fromConfig(config: Config, datadir: File): EclairAuthCredentials =
    fromConfig(config, Some(datadir))

  /**
    * $fromConfigDoc
    */
  def fromConfig(config: Config): EclairAuthCredentials =
    fromConfig(config, None)

  private[config] def fromConfig(
      config: Config,
      datadir: Option[File]): EclairAuthCredentials = {

    val bitcoindUsername = config.getString("eclair.bitcoind.rpcuser")
    val bitcoindPassword = config.getString("eclair.bitcoind.rpcpassword")

    val defaultBitcoindPort = getDefaultBitcoindRpcPort(config)
    val bitcoindRpcPort =
      ConfigUtil.getIntOrElse(config,
                              "eclair.bitcoind.rpcport",
                              defaultBitcoindPort)

    val bitcoindRpcHost =
      ConfigUtil.getStringOrElse(config, "eclair.bitcoind.host", "localhost")

    val bitcoindUri = new URI(s"http://$bitcoindRpcHost:$bitcoindRpcPort")

    //does eclair not have a username field??
    val password = config.getString("eclair.api.password")
    val eclairRpcPort = ConfigUtil.getIntOrElse(config, "eclair.api.port", 8080)

    val bitcoindAuth = {
      BitcoindAuthCredentials.PasswordBased(username = bitcoindUsername,
                                            password = bitcoindPassword)
    }

    EclairAuthCredentials(password = password,
                          bitcoinAuthOpt = Some(bitcoindAuth),
                          rpcPort = eclairRpcPort,
                          bitcoindRpcUri = bitcoindUri,
                          datadir = datadir)
  }

  private def getDefaultBitcoindRpcPort(config: Config): Int = {
    val network = ConfigUtil.getStringOrElse(config, "eclair.chain", "testnet")
    network match {
      case "mainnet" => MainNet.rpcPort
      case "testnet" => TestNet3.rpcPort
      case "regtest" => RegTest.rpcPort
      case _: String =>
        throw new IllegalArgumentException(
          s"Got invalid chain parameter $network ")
    }
  }

}
