package org.bitcoins.node

import akka.actor.{ActorRef, ActorSystem}
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.{
  PeerMessageReceiver,
  PeerMessageSender
}

import scala.concurrent.duration.DurationInt

/** PeerData contains objects specific to a peer associated together
  */
case class PeerData(
    peer: Peer,
    node: Node,
    supervisor: ActorRef
)(implicit system: ActorSystem, nodeAppConfig: NodeAppConfig) {

  lazy val peerMessageSender: PeerMessageSender = PeerMessageSender(client)

  lazy val client: P2PClient = {
    val peerMessageReceiver =
      PeerMessageReceiver.newReceiver(node = node, peer = peer)
    P2PClient(
      peer = peer,
      peerMessageReceiver = peerMessageReceiver,
      onReconnect = node.peerManager.onReconnect,
      onStop = node.peerManager.onP2PClientStopped,
      maxReconnectionTries = 4,
      supervisor = supervisor
    )
  }

  private var _serviceIdentifier: Option[ServiceIdentifier] = None

  def serviceIdentifier: ServiceIdentifier = {
    _serviceIdentifier.getOrElse(
      throw new RuntimeException(
        s"Tried using ServiceIdentifier for uninitialized peer $peer"))
  }

  def setServiceIdentifier(serviceIdentifier: ServiceIdentifier): Unit = {
    _serviceIdentifier = Some(serviceIdentifier)
  }

  private var lastTimedOut: Long = 0

  def updateLastFailureTime(): Unit = {
    lastTimedOut = System.currentTimeMillis()
  }

  /** returns true if the peer has failed due to any reason within the past 30 minutes
    */
  def hasFailedRecently: Boolean = {
    val timePast = System.currentTimeMillis() - lastTimedOut
    timePast < 30.minutes.toMillis
  }
}
