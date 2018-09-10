package org.bitcoins.eclair.rpc.config

import java.net.URI

import org.bitcoins.core.config.NetworkParameters

sealed trait DaemonInstance {
  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def bitcoinRpcUri: URI
  def authCredentials: AuthCredentials
}

object DaemonInstance {
  private case class DaemonInstanceImpl(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    bitcoinRpcUri: URI,
    authCredentials: AuthCredentials) extends DaemonInstance

  def apply(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    bitcoinRpcUri: URI,
    authCredentials: AuthCredentials): DaemonInstance = {
    DaemonInstanceImpl(network, uri, rpcUri, bitcoinRpcUri, authCredentials)
  }
}
