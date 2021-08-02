package org.bitcoins.testkit.util

import grizzled.slf4j.Logging

import java.net.{InetSocketAddress, Socket}
import scala.util.Try

object NetworkUtil extends Logging {

  def portIsBound(address: InetSocketAddress): Boolean =
    Try {
      val socket = new Socket(address.getHostString, address.getPort)
      socket.close()
    }.isSuccess
}
