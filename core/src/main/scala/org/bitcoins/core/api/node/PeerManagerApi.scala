package org.bitcoins.core.api.node

import scala.concurrent.Future

trait PeerManagerApi {

  def disconnectPeer(peer: Peer): Future[Unit]

  def connectPeer(peer: Peer): Future[Unit]

  def isConnected(peer: Peer): Future[Boolean]

  def isDisconnected(peer: Peer): Future[Boolean]

  def isInitialized(peer: Peer): Future[Boolean]

}
