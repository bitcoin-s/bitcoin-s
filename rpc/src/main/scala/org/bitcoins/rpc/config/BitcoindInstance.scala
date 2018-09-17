package org.bitcoins.rpc.config

import java.net.URI

import org.bitcoins.core.config.NetworkParameters

/**
 * Created by chris on 4/29/17.
 */
sealed trait BitcoindInstance {

  def network: NetworkParameters
  def uri: URI
  def rpcUri: URI
  def authCredentials: BitcoindAuthCredentials

  def zmqPortOpt: Option[Int]
}

object BitcoindInstance {
  private case class BitcoindInstanceImpl(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: BitcoindAuthCredentials,
    zmqPortOpt: Option[Int]) extends BitcoindInstance

  def apply(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: BitcoindAuthCredentials,
    zmqPortOpt: Option[Int]): BitcoindInstance = {
    BitcoindInstanceImpl(network, uri, rpcUri, authCredentials, zmqPortOpt)
  }

  /** Create a bitcoind instance with no zmq port specified */
  def apply(
    network: NetworkParameters,
    uri: URI,
    rpcUri: URI,
    authCredentials: BitcoindAuthCredentials): BitcoindInstance = {
    BitcoindInstance(network, uri, rpcUri, authCredentials, None)
  }
}
