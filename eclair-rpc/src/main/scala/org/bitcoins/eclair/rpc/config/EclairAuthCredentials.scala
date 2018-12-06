package org.bitcoins.eclair.rpc.config

import java.io.File

import com.typesafe.config.{ Config, ConfigFactory }
import org.bitcoins.rpc.config.BitcoindAuthCredentials

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
  def bitcoinRpcPort: Option[Int] = {
    bitcoinAuthOpt.map(_.rpcPort)
  }

  /** `eclair.api.password` field in our `eclair.conf` file */
  def password: String

  /** The port for eclair's rpc client */
  def port: Int

  def copyWithDatadir(datadir: File): EclairAuthCredentials = {
    EclairAuthCredentials(
      password = password,
      bitcoinAuthOpt = bitcoinAuthOpt,
      port = port,
      datadir = Some(datadir))
  }
}

object EclairAuthCredentials {
  private case class AuthCredentialsImpl(
    password: String,
    bitcoinAuthOpt: Option[BitcoindAuthCredentials],
    port: Int,
    datadir: Option[File]) extends EclairAuthCredentials

  def apply(
    password: String,
    bitcoinAuthOpt: Option[BitcoindAuthCredentials],
    port: Int): EclairAuthCredentials = {
    EclairAuthCredentials(password, bitcoinAuthOpt, port, None)
  }

  def apply(
    password: String,
    bitcoinAuthOpt: Option[BitcoindAuthCredentials],
    port: Int,
    datadir: Option[File]): EclairAuthCredentials = {
    AuthCredentialsImpl(password, bitcoinAuthOpt, port, datadir)
  }

  def fromDatadir(datadir: File): EclairAuthCredentials = {
    val confFile = new File(datadir.getAbsolutePath + "/eclair.conf")
    val config = ConfigFactory.parseFile(confFile)
    val auth = fromConfig(config)
    auth.copyWithDatadir(datadir = datadir)
  }

  /**
   * Parses a [[com.typesafe.config.Config Config]] in the format of this
   * [[https://github.com/ACINQ/eclair/blob/master/eclair-core/src/main/resources/reference.conf sample reference.conf]]
   * file to a
   * [[org.bitcoins.eclair.rpc.config.EclairAuthCredentials EclairAuthCredentials]]
   */
  def fromConfig(config: Config): EclairAuthCredentials = {

    val bitcoindUsername = config.getString("eclair.bitcoind.rpcuser")
    val bitcoindPassword = config.getString("eclair.bitcoind.rpcpassword")
    val bitcoindRpcPort = config.getInt("eclair.bitcoind.rpcport")

    //does eclair not have a username field??
    val password = config.getString("eclair.api.password")
    val eclairRpcPort = ConfigUtil.getIntOrElse(config, "eclair.api.port", 8080)

    val bitcoindAuth = {
      BitcoindAuthCredentials(
        username = bitcoindUsername,
        password = bitcoindPassword,
        rpcPort = bitcoindRpcPort)
    }

    EclairAuthCredentials(
      password = password,
      bitcoinAuthOpt = Some(bitcoindAuth),
      port = eclairRpcPort)
  }

}
