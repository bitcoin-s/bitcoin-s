package org.bitcoins.testkit.util

import grizzled.slf4j.Logging
import org.bitcoins.testkit.util.NetworkUtil._

import java.net.InetSocketAddress
import scala.util.Properties

object TorUtil extends Logging {

  val torEnabled: Boolean = Properties
    .envOrNone("TOR")
    .isDefined

  def torProxyAddress = new InetSocketAddress("localhost", 9050)
  def torControlAddress = new InetSocketAddress("localhost", 9051)
  def torProxyEnabled: Boolean = portIsBound(torProxyAddress)
  def torControlEnabled: Boolean = portIsBound(torControlAddress)

  def verifyTorEnabled(): Unit = {
    assume(torProxyEnabled, "Tor daemon is not running or listening port 9050")
    assume(torControlEnabled,
           "Tor daemon is not running or listening port 9051")
  }
}
