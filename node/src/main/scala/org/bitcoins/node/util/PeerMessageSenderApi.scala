package org.bitcoins.node.util

import org.bitcoins.core.p2p.{NetworkPayload, TypeIdentifier}
import org.bitcoins.crypto.{DoubleSha256DigestBE}
import org.bitcoins.node.models.Peer

import scala.concurrent.Future

trait PeerMessageSenderApi {

  def sendGetDataMessage(
      typeIdentifier: TypeIdentifier,
      hash: DoubleSha256DigestBE,
      peerOpt: Option[Peer]): Future[Unit] = {
    sendGetDataMessages(typeIdentifier, Vector(hash), peerOpt)
  }

  def sendGetDataMessages(
      typeIdentifier: TypeIdentifier,
      hashes: Vector[DoubleSha256DigestBE],
      peerOpt: Option[Peer]): Future[Unit]

  def sendGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE],
      peerOpt: Option[Peer]): Future[Unit]

  /** Gossips the given message to all peers except the excluded peer. If None given as excluded peer, gossip message to all peers */
  def gossipMessage(
      msg: NetworkPayload,
      excludedPeerOpt: Option[Peer]): Future[Unit]

  def sendMsg(msg: NetworkPayload, peerOpt: Option[Peer]): Future[Unit]

}
