package org.bitcoins.node.models

import java.net.InetSocketAddress

import org.bitcoins.core.api.db.DbRowAutoInc
import org.bitcoins.rpc.config.BitcoindInstance

case class Peer(socket: InetSocketAddress, id: Option[Long] = None)
    extends DbRowAutoInc[Peer] {

  override def copyWithId(id: Long): Peer = {
    this.copy(id = Some(id))
  }

  override def toString(): String =
    s"Peer(${socket.getAddress()}:${socket.getPort()})"

}

object Peer {

  def fromSocket(socket: InetSocketAddress): Peer = {
    Peer(socket)
  }

  /** Constructs a peer from the given `bitcoind` instance
    */
  def fromBitcoind(bitcoind: BitcoindInstance): Peer = {
    val socket = new InetSocketAddress(bitcoind.uri.getHost, bitcoind.p2pPort)
    fromSocket(socket)
  }
}
