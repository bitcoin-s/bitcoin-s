package org.bitcoins.tor

import org.bitcoins.tor.TorProtocolHandler.Authentication

import java.net.InetSocketAddress
import java.nio.file.Path

case class TorParams(
    controlAddress: InetSocketAddress,
    authentication: Authentication,
    privateKeyPath: Path
)

object TorParams {
  val DefaultProxyPort: Int = 9050
  val DefaultControlPort: Int = 9051
}
