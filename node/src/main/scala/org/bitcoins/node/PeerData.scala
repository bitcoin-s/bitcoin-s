package org.bitcoins.node

import akka.actor.ActorSystem
import akka.stream.scaladsl.SourceQueueWithComplete
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer._
import org.bitcoins.node.util.PeerMessageSenderApi

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** PeerData contains objects specific to a peer associated together
  */
sealed trait PeerData {

  implicit protected def nodeAppConfig: NodeAppConfig
  implicit protected def chainAppConfig: ChainAppConfig

  implicit protected def system: ActorSystem
  def peer: Peer
  def controlMessageHandler: ControlMessageHandler

  def queue: SourceQueueWithComplete[NodeStreamMessage]

  def peerMessageSenderApi: PeerMessageSenderApi

  private val initPeerMessageRecv = PeerMessageReceiver(
    controlMessageHandler = controlMessageHandler,
    queue = queue,
    peer = peer,
    state = PeerMessageReceiverState.fresh())

  def stop(): Future[Unit] = {
    peerMessageSender.disconnect()
  }

  val peerMessageSender: PeerMessageSender = {
    PeerMessageSender(peer, initPeerMessageRecv, peerMessageSenderApi)
  }

  private[this] var _serviceIdentifier: Option[ServiceIdentifier] = None

  def serviceIdentifier: ServiceIdentifier = {
    _serviceIdentifier.getOrElse(
      throw new RuntimeException(
        s"Tried using ServiceIdentifier for uninitialized peer $peer"))
  }

  def setServiceIdentifier(serviceIdentifier: ServiceIdentifier): Unit = {
    _serviceIdentifier = Some(serviceIdentifier)
  }
}

/** A peer we plan on being connected to persistently */
case class PersistentPeerData(
    peer: Peer,
    controlMessageHandler: ControlMessageHandler,
    queue: SourceQueueWithComplete[NodeStreamMessage],
    peerMessageSenderApi: PeerMessageSenderApi
)(implicit
    override val system: ActorSystem,
    override val nodeAppConfig: NodeAppConfig,
    override val chainAppConfig: ChainAppConfig)
    extends PeerData {

  private var _invalidMessagesCount: Int = 0

  def updateInvalidMessageCount(): Unit = {
    _invalidMessagesCount += 1
  }

  def getInvalidMessageCount = _invalidMessagesCount

  private var lastTimedOut: Long = 0

  def updateLastFailureTime(): Unit = {
    lastTimedOut = System.currentTimeMillis()
  }

  def isConnectionTimedOut: Boolean = peerMessageSender.isConnectionTimedOut

  /** returns true if the peer has failed due to any reason within the past 30 minutes
    */
  def hasFailedRecently: Boolean = {
    val timePast = System.currentTimeMillis() - lastTimedOut
    timePast < 30.minutes.toMillis
  }

  def exceededMaxInvalidMessages: Boolean = {
    _invalidMessagesCount > nodeAppConfig.maxInvalidResponsesAllowed
  }
}

/** A peer we are just discovering on the p2p network for future connections
  * we do not want to be persistently connected to this peer, just see if
  * we can connect to it and exchange version/verack messages
  */
case class AttemptToConnectPeerData(
    peer: Peer,
    controlMessageHandler: ControlMessageHandler,
    queue: SourceQueueWithComplete[NodeStreamMessage],
    peerMessageSenderApi: PeerMessageSenderApi
)(implicit
    override val system: ActorSystem,
    override val nodeAppConfig: NodeAppConfig,
    override val chainAppConfig: ChainAppConfig)
    extends PeerData
