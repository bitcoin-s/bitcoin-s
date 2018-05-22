package org.bitcoins.rpc.config

import java.net.URI

import org.bitcoins.core.config.NetworkParameters

/**
 * Created by chris on 4/29/17.
 */
sealed trait DaemonInstance {

  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: AuthCredentials
}

object DaemonInstance {
  private case class DaemonInstanceImpl(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: AuthCredentials)
    extends DaemonInstance

  def apply(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: AuthCredentials): DaemonInstance = {
    DaemonInstanceImpl(network, uri, rpcUri, authCredentials)
  }
}
