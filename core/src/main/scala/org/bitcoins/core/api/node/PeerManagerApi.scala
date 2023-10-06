package org.bitcoins.core.api.node

import org.bitcoins.core.p2p.NetworkPayload
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

trait PeerManagerApi {

  /** Peers we are currently connected too and have completed the version/verack handshake with */
  def peers: Set[Peer]

  def disconnectPeer(peer: Peer): Future[Unit]

  def connectPeer(peer: Peer): Future[Unit]

  def isConnected(peer: Peer): Future[Boolean]

  def isDisconnected(peer: Peer): Future[Boolean]

  def isInitialized(peer: Peer): Future[Boolean]

  /** Gossips the given message to all peers except the excluded peer. If None given as excluded peer, gossip message to all peers */
  def gossipMessage(msg: NetworkPayload, excludedPeerOpt: Option[Peer]): Future[Unit]

  /** Gossips the [[org.bitcoins.core.p2p.GetHeadersMessage]] to all of our peers to attempt ot get the best block headers */
  def gossipGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE]): Future[Unit]

  def sendToRandomPeer(payload: NetworkPayload): Future[Unit]
}
