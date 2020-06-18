package org.bitcoins.core.util

import java.net.InetSocketAddress

abstract class NetworkUtil {

  private def parsePort(port: String): Int = {
    lazy val errorMsg = s"Invalid peer port: $port"
    try {
      val res = port.toInt
      if (res < 0 || res > 0xffff) {
        throw new RuntimeException(errorMsg)
      }
      res
    } catch {
      case _: NumberFormatException =>
        throw new RuntimeException(errorMsg)
    }
  }

  /** Parses a string that looks like this to [[java.net.InetSocketAddress]]
    * "neutrino.testnet3.suredbits.com:18333"
    */
  def parseInetSocketAddress(
      address: String,
      defaultPort: Int): InetSocketAddress = {
    address.split(":") match {
      case Array(host)       => new InetSocketAddress(host, defaultPort)
      case Array(host, port) => new InetSocketAddress(host, parsePort(port))
      case _                 => throw new RuntimeException(s"Invalid peer address: $address")
    }
  }
}

object NetworkUtil extends NetworkUtil
