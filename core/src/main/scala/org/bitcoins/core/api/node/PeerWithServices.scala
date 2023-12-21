package org.bitcoins.core.api.node

import org.bitcoins.core.api.tor.Socks5ProxyParams
import org.bitcoins.core.p2p.ServiceIdentifier

import java.net.InetSocketAddress

case class PeerWithServices(peer: Peer, services: ServiceIdentifier) {
  val id: Option[Long] = peer.id
  val socket: InetSocketAddress = peer.socket
  val socks5ProxyParams: Option[Socks5ProxyParams] = peer.socks5ProxyParams
}
