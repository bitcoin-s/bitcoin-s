package org.bitcoins.testkit.util

import grizzled.slf4j.Logging
import org.bitcoins.core.util.NetworkUtil.portIsBound
import org.bitcoins.tor.TorParams

import java.net.{InetAddress, InetSocketAddress}
import scala.util.Properties

object TorUtil extends Logging {

  val torEnabled: Boolean = Properties
    .envOrNone("TOR")
    .isDefined

  def torProxyAddress =
    new InetSocketAddress(InetAddress.getLoopbackAddress,
                          TorParams.DefaultProxyPort)

  def torControlAddress =
    new InetSocketAddress(InetAddress.getLoopbackAddress,
                          TorParams.DefaultControlPort)

  def torProxyEnabled: Boolean = portIsBound(torProxyAddress)
  def torControlEnabled: Boolean = portIsBound(torControlAddress)

  def verifyTorEnabled(): Unit = {
    assume(
      torProxyEnabled,
      s"Tor daemon is not running or listening port ${TorParams.DefaultProxyPort}")
    assume(
      torControlEnabled,
      s"Tor daemon is not running or listening port ${TorParams.DefaultControlPort}")
  }
}
