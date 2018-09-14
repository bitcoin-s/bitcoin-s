package org.bitcoins.eclair.rpc.config

import java.net.URI

import org.bitcoins.core.config.NetworkParameters

sealed trait EclairInstance {
  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: EclairAuthCredentials
}

object EclairInstance {
  private case class EclairInstanceImpl(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: EclairAuthCredentials) extends EclairInstance

  def apply(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: EclairAuthCredentials): EclairInstance = {
    EclairInstanceImpl(network, uri, rpcUri, authCredentials)
  }
}
