package org.bitcoins.core.util

import java.net.{InetSocketAddress, URI}

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
}

object NetworkUtil extends NetworkUtil
