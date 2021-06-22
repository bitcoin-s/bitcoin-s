package org.bitcoins.node.models

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.rpc.config.BitcoindInstance
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

  override def toString(): String =
    s"Peer(${socket.getHostString()}:${socket.getPort()})"

}

object Peer {

  def fromSocket(
      socket: InetSocketAddress,
      socks5ProxyParams: Option[Socks5ProxyParams] = None): Peer = {
    Peer(socket, socks5ProxyParams = socks5ProxyParams)
  }

  /** Constructs a peer from the given `bitcoind` instance
    */
  def fromBitcoind(
      bitcoind: BitcoindInstance,
      socks5ProxyParams: Option[Socks5ProxyParams] = None): Peer = {
    val socket = new InetSocketAddress(bitcoind.uri.getHost, bitcoind.p2pPort)
    fromSocket(socket, socks5ProxyParams = socks5ProxyParams)
  }
}
