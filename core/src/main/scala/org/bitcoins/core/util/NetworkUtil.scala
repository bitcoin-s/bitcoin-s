package org.bitcoins.core.util

import java.net._
import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

abstract class NetworkUtil {

  /** Parses a string that looks like this to [[java.net.InetSocketAddress]]
    * "neutrino.testnet3.suredbits.com:18333"
    */
  def parseInetSocketAddress(
      address: String,
      defaultPort: => Int): InetSocketAddress = {
    val uri = new URI("tcp://" + address)
    val port = if (uri.getPort < 0) defaultPort else uri.getPort
    InetSocketAddress.createUnresolved(uri.getHost, port)
  }

  def isLocalhost(hostName: String): Boolean = {
    hostName == "127.0.0.0" || hostName == "localhost"
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

  def portIsBound(address: InetSocketAddress): Boolean =
    Try {
      val socket = new Socket(address.getHostString, address.getPort)
      socket.close()
    }.isSuccess

  /** Generates a random port not in use
    */
  @tailrec
  final def randomPort(): Int = {
    val MAX = 65535 // max tcp port number
    val MIN = 1025 // lowest port not requiring sudo
    val port = Math.abs(Random.nextInt(MAX - MIN) + (MIN + 1))
    val attempt = Try {
      val socket = new ServerSocket(port)
      socket.close()
      socket.getLocalPort
    }

    attempt match {
      case Success(value) => value
      case Failure(_)     => randomPort()
    }
  }

}

object NetworkUtil extends NetworkUtil
