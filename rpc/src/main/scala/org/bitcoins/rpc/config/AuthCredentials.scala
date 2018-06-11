package org.bitcoins.rpc.config

/**
  * Created by chris on 5/2/17.
  */
sealed trait AuthCredentials {

  /** The directory where our bitcoin.conf file is located */
  def datadir: String

  /** rpcusername field in our bitcoin.conf file */
  def username: String

  /** rpcpassword field in our bitcoin.conf file */
  def password: String
}

object AuthCredentials {
  private case class AuthCredentialsImpl(
      username: String,
      password: String,
      datadir: String)
      extends AuthCredentials

  def apply(username: String, password: String): AuthCredentials = {
    val defaultDataDir = System.getProperty("user.home") + "/.bitcoin"
    AuthCredentials(username, password, defaultDataDir)
  }

  def apply(
      username: String,
      password: String,
      datadir: String): AuthCredentials = {
    AuthCredentialsImpl(username, password, datadir)
  }
}
