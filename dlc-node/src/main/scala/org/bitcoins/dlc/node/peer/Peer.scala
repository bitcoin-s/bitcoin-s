package org.bitcoins.dlc.node.peer

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.tor.Socks5ProxyParams

import java.net.InetSocketAddress

case class Peer(
    socket: InetSocketAddress,
    id: Option[Long] = None,
    socks5ProxyParams: Option[Socks5ProxyParams] = None)
    extends DbRowAutoInc[Peer] {

  override def copyWithId(id: Long): Peer = {
    this.copy(id = Some(id))
  }

  override def toString: String =
    s"Peer(${socket.getHostString}:${socket.getPort})"

}

object Peer {

  def fromSocket(
      socket: InetSocketAddress,
      socks5ProxyParams: Option[Socks5ProxyParams] = None): Peer = {
    Peer(socket, socks5ProxyParams = socks5ProxyParams)
  }
}
