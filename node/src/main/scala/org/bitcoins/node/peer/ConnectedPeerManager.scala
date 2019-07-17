package org.bitcoins.node.peer

import org.bitcoins.node.models.Peer
import org.bitcoins.db.P2PLogger
import org.bitcoins.chain.config.ChainAppConfig

/** Manages all peers we have active connections with */
class ConnectedPeerManager extends P2PLogger {
  implicit val config: ChainAppConfig = ???
  private var connectedPeers: Vector[Peer] = Vector.empty

  def add(peer: Peer): ConnectedPeerManager = {
    logger.debug(s"Adding peer=${peer} to connected peers")

    connectedPeers = connectedPeers.+:(peer)

    this
  }

  def delete(peer: Peer): Option[ConnectedPeerManager] = {
    val currentPeers = connectedPeers
    val newPeers = currentPeers.filterNot(_ == peer)
    if (newPeers.length != currentPeers.length) {
      connectedPeers = newPeers
      Some(this)
    } else {
      None
    }
  }

  def empty: ConnectedPeerManager = {
    new ConnectedPeerManager
  }
}
