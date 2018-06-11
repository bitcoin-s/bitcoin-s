package org.bitcoins.rpc.config

import java.net.{InetAddress, InetSocketAddress}

import org.bitcoins.core.config.NetworkParameters

/**
  * Created by chris on 4/29/17.
  */
sealed trait DaemonInstance {

  def network: NetworkParameters
  def uri: InetSocketAddress
  def rpcUri: InetSocketAddress
  def authCredentials: AuthCredentials
}

object DaemonInstance {
  private case class DaemonInstanceImpl(
      network: NetworkParameters,
      uri: InetSocketAddress,
      rpcUri: InetSocketAddress,
      authCredentials: AuthCredentials)
      extends DaemonInstance

  def apply(
      network: NetworkParameters,
      uri: InetSocketAddress,
      rpcUri: InetSocketAddress,
      authCredentials: AuthCredentials): DaemonInstance = {
    DaemonInstanceImpl(network, uri, rpcUri, authCredentials)
  }
}
