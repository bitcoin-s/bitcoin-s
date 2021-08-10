package org.bitcoins.core.util

import java.net.{InetAddress, InetSocketAddress, URI, UnknownHostException}

abstract class NetworkUtil {

  /** Parses a string that looks like this to [[java.net.InetSocketAddress]]
    * "neutrino.testnet3.suredbits.com:18333"
    */
  def parseInetSocketAddress(
      address: String,
      defaultPort: Int): InetSocketAddress = {
    val uri = new URI("tcp://" + address)
    val port = if (uri.getPort < 0) defaultPort else uri.getPort
    InetSocketAddress.createUnresolved(uri.getHost, port)
  }

  /** Checks if the [[java.net.InetSocketAddress]] is a loopback address.
    *
    * @return a boolean indicating if the [[java.net.InetSocketAddress]] is
    *         a loopback address; or false otherwise.
    */
  def isLoopbackAddress(socketAddress: InetSocketAddress): Boolean = {
    try {
      val addr = if (socketAddress.getAddress == null) {
        // the address is unresolved, try to resolve it
        InetAddress.getByName(socketAddress.getHostString)
      } else {
        socketAddress.getAddress
      }
      addr.isLoopbackAddress
    } catch {
      case _: UnknownHostException =>
        // loopback addresses should be always resolved
        // if we have a resolver error, that means the address is definitely not a loopback one
        false
    }
  }

  /** Checks if the [[java.net.URI]] is pointing to a loopback address.
    *
    * @return a boolean indicating if the [[java.net.URI]] is
    *         a loopback address; or false otherwise.
    */
  def isLoopbackAddress(uri: URI): Boolean = {
    try {
      //  try to resolve the address
      val addr = InetAddress.getByName(uri.getHost)
      addr.isLoopbackAddress
    } catch {
      case _: UnknownHostException =>
        // loopback addresses should be always resolved
        // if we have a resolver error (ex. for an onion address),
        // that means the address is definitely not a loopback one
        false
    }
  }
}

object NetworkUtil extends NetworkUtil
