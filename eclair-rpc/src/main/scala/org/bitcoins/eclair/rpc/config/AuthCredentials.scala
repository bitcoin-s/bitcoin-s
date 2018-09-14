package org.bitcoins.eclair.rpc.config

import java.io.File

sealed trait EclairAuthCredentials {

  /** The directory where our eclair.conf file is located */
  def datadir: File

  /** rpcusername field in our bitcoin.conf file */
  def bitcoinUsername: String

  /** rpcpassword field in our bitcoin.conf file */
  def bitcoinPassword: String

  /** alias field in our eclair.conf file */
  def username: String

  /** api password field in our eclair.conf file */
  def password: String
}

object EclairAuthCredentials {
  private case class AuthCredentialsImpl(
    username: String,
    password: String,
    bitcoinUsername: String,
    bitcoinPassword: String,
    datadir: File) extends EclairAuthCredentials

  def apply(
    username: String,
    password: String,
    bitcoinUsername: String,
    bitcoinPassword: String): EclairAuthCredentials = {
    val defaultDataDir = new File(System.getProperty("user.home") + "/.eclair")
    EclairAuthCredentials(username, password, bitcoinUsername, bitcoinPassword, defaultDataDir)
  }

  def apply(
    username: String,
    password: String,
    bitcoinUsername: String,
    bitcoinPassword: String,
    datadir: File): EclairAuthCredentials = {
    AuthCredentialsImpl(username, password, bitcoinUsername, bitcoinPassword, datadir)
  }
}
