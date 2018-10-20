package org.bitcoins.eclair.rpc.config

import java.io.File

import com.typesafe.config.Config
import org.bitcoins.rpc.config.BitcoindAuthCredentials

sealed trait EclairAuthCredentials {

  /** The directory where our eclair.conf file is located */
  def datadir: Option[File]

  def bitcoinAuthOpt: Option[BitcoindAuthCredentials]

  /** rpcusername field in our bitcoin.conf file */
  def bitcoinUsername: Option[String] = {
    bitcoinAuthOpt.map(_.username)
  }

  /** rpcpassword field in our bitcoin.conf file */
  def bitcoinPassword: Option[String] = {
    bitcoinAuthOpt.map(_.password)
  }

  def bitcoinRpcPort: Option[Int] = {
    bitcoinAuthOpt.map(_.rpcPort)
  }

  /** alias field in our eclair.conf file */
  def username: String

  /** api password field in our eclair.conf file */
  def password: String

  /** The port for eclair's rpc client */
  def port: Int
}

object EclairAuthCredentials {
  private case class AuthCredentialsImpl(
    username: String,
    password: String,
    bitcoinAuthOpt: Option[BitcoindAuthCredentials],
    port: Int,
    datadir: Option[File]) extends EclairAuthCredentials

  def apply(
    username: String,
    password: String,
    bitcoinAuthOpt: Option[BitcoindAuthCredentials],
    port: Int): EclairAuthCredentials = {
    EclairAuthCredentials(username, password, bitcoinAuthOpt, port, None)
  }

  def apply(
    username: String,
    password: String,
    bitcoinAuthOpt: Option[BitcoindAuthCredentials],
    port: Int,
    datadir: Option[File]): EclairAuthCredentials = {
    AuthCredentialsImpl(username, password, bitcoinAuthOpt, port, datadir)
  }

  /**
   * Parses a config in the format of this to a [[EclairAuthCredentials]]
   * [[https://github.com/ACINQ/eclair/blob/master/eclair-core/src/main/resources/reference.conf]]
   * @param config
   * @return
   */
  def fromConfig(config: Config): EclairAuthCredentials = {
    //does eclair not have a username field??
    val username = config.getString("eclair.alias")
    val password = config.getString("eclair.api.password")
    val bitcoindUsername = config.getString("eclair.bitcoind.rpcuser")
    val bitcoindPassword = config.getString("eclair.bitcoind.rpcpassword")
    val bitcoindRpcPort = config.getInt("eclair.bitcoind.rpcport")
    val eclairRpcPort = config.getInt("eclair.api.port")

    val bitcoindAuth = {
      BitcoindAuthCredentials(
        username = bitcoindUsername,
        password = bitcoindPassword,
        rpcPort = bitcoindRpcPort)
    }

    EclairAuthCredentials(
      username = username,
      password = password,
      bitcoinAuthOpt = Some(bitcoindAuth),
      port = eclairRpcPort)
  }
}
