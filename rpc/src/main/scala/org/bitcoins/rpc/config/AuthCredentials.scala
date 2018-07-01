package org.bitcoins.rpc.config

import java.io.File

/**
  * Created by chris on 5/2/17.
  */
sealed trait AuthCredentials {

  /** The directory where our bitcoin.conf file is located */
  def datadir: File

  /** rpcusername field in our bitcoin.conf file */
  def username: String

  /** rpcpassword field in our bitcoin.conf file */
  def password: String
}

object AuthCredentials {
  private case class AuthCredentialsImpl(
      username: String,
      password: String,
      datadir: File)
      extends AuthCredentials

  def apply(username: String, password: String): AuthCredentials = {
    val defaultDataDir = new File(System.getProperty("user.home") + "/.bitcoin")
    AuthCredentials(username, password, defaultDataDir)
  }

  def apply(
      username: String,
      password: String,
      datadir: File): AuthCredentials = {
    AuthCredentialsImpl(username, password, datadir)
  }
}
