package org.bitcoins.core.api.node

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.core.api.tor.Socks5ProxyParams

import java.net.{InetSocketAddress, URI}

case class Peer(
    socket: InetSocketAddress,
    socks5ProxyParams: Option[Socks5ProxyParams],
    id: Option[Long] = None
) extends DbRowAutoInc[Peer] {

  override def copyWithId(id: Long): Peer = {
    this.copy(id = Some(id))
  }

  def port: Int = socket.getPort

  override def toString(): String =
    s"Peer(${socket.getHostString()}:${socket.getPort()})"

}

object Peer {

  def fromSocket(
      socket: InetSocketAddress,
      socks5ProxyParams: Option[Socks5ProxyParams]): Peer = {
    Peer(socket, socks5ProxyParams = socks5ProxyParams)
  }

  def fromURI(
      uri: URI,
      socks5ProxyParamsOpt: Option[Socks5ProxyParams]): Peer = {
    val socket = new InetSocketAddress(uri.getHost, uri.getPort)
    fromSocket(socket, socks5ProxyParamsOpt)
  }
}
