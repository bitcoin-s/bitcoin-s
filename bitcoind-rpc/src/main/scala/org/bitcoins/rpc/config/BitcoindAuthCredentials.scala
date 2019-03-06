package org.bitcoins.rpc.config

import java.io.File

/**
  * Created by chris on 5/2/17.
  */
sealed trait BitcoindAuthCredentials {

  /** The directory where our bitcoin.conf file is located */
  def datadir: File

  /** rpcusername field in our bitcoin.conf file */
  def username: String

  /** rpcpassword field in our bitcoin.conf file */
  def password: String

  def rpcPort: Int
}

object BitcoindAuthCredentials {
  private case class BitcoindAuthCredentialsImpl(
      username: String,
      password: String,
      rpcPort: Int,
      datadir: File)
      extends BitcoindAuthCredentials

  def apply(
      username: String,
      password: String,
      rpcPort: Int): BitcoindAuthCredentials = {
    val defaultDataDir = new File(System.getProperty("user.home") + "/.bitcoin")
    BitcoindAuthCredentials(username, password, rpcPort, defaultDataDir)
  }

  def apply(
      username: String,
      password: String,
      rpcPort: Int,
      datadir: File): BitcoindAuthCredentials = {
    BitcoindAuthCredentialsImpl(username, password, rpcPort, datadir)
  }
}
