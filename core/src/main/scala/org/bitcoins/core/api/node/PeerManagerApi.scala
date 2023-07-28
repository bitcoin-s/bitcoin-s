package org.bitcoins.core.api.node

import scala.concurrent.Future

trait PeerManagerApi {

  /** Peers we are currently connected too and have completed the version/verack handshake with */
  def peers: Set[Peer]

  def disconnectPeer(peer: Peer): Future[Unit]

  def connectPeer(peer: Peer): Future[Unit]

  def isConnected(peer: Peer): Future[Boolean]

  def isDisconnected(peer: Peer): Future[Boolean]

  def isInitialized(peer: Peer): Future[Boolean]

}
