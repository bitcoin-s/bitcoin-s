package org.bitcoins.rpc.config

import java.net.URI

import org.bitcoins.core.config.NetworkParameters

/**
  * Created by chris on 4/29/17.
  */
sealed trait BitcoindInstance {
  require(
    rpcUri.getPort == rpcPort,
    s"RpcUri and the rpcPort in authCredentials are different ${rpcUri} authcred: ${rpcPort}")
  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: BitcoindAuthCredentials

  def rpcPort: Int = authCredentials.rpcPort
  def zmqPortOpt: Option[Int]
}

object BitcoindInstance {
  private case class BitcoindInstanceImpl(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqPortOpt: Option[Int])
      extends BitcoindInstance

  def apply(
      network: NetworkParameters,
      uri: URI,
      rpcUri: URI,
      authCredentials: BitcoindAuthCredentials,
      zmqPortOpt: Option[Int] = None): BitcoindInstance = {
    BitcoindInstanceImpl(network, uri, rpcUri, authCredentials, zmqPortOpt)
  }
}
