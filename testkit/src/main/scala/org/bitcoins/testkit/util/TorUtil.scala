package org.bitcoins.testkit.util

import grizzled.slf4j.Logging
import org.bitcoins.testkit.util.NetworkUtil._

import java.net.InetSocketAddress
import scala.util.Properties

object TorUtil extends Logging {

  val torEnabled: Boolean = Properties
    .envOrNone("TOR")
    .isDefined

  val PROXY_PORT = 9050
  val CONTROL_PORT = 9051

  def torProxyAddress = new InetSocketAddress("localhost", PROXY_PORT)
  def torControlAddress = new InetSocketAddress("localhost", CONTROL_PORT)
  def torProxyEnabled: Boolean = portIsBound(torProxyAddress)
  def torControlEnabled: Boolean = portIsBound(torControlAddress)

  def verifyTorEnabled(): Unit = {
    assume(torProxyEnabled, "Tor daemon is not running or listening port 9050")
    assume(torControlEnabled,
           "Tor daemon is not running or listening port 9051")
  }
}
