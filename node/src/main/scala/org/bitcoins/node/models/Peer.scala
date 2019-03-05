package org.bitcoins.node.models

import java.net.InetSocketAddress

import org.bitcoins.core.number.UInt32
import org.bitcoins.db.DbRowAutoInc
import org.bitcoins.node.util.NetworkIpAddress

case class Peer(id: Option[Long], networkIpAddress: NetworkIpAddress)
    extends DbRowAutoInc[Peer] {

  def socket: InetSocketAddress =
    new InetSocketAddress(networkIpAddress.address, networkIpAddress.port)

  override def copyWithId(id: Long): Peer = {
    this.copy(id = Some(id))
  }

}

object Peer {

  def fromNetworkIpAddress(networkIpAddress: NetworkIpAddress): Peer = {
    Peer(None, networkIpAddress)
  }

  def fromSocket(socket: InetSocketAddress): Peer = {
    val nip = NetworkIpAddress.fromInetSocketAddress(socket = socket)
    fromNetworkIpAddress(nip)
  }
}
