package org.bitcoins.node

import akka.actor.ActorSystem
import akka.stream.scaladsl.SourceQueueWithComplete
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer._
import org.bitcoins.node.util.PeerMessageSenderApi

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** PeerData contains objects specific to a peer associated together
  */
case class PeerData(
    peer: Peer,
    controlMessageHandler: ControlMessageHandler,
    queue: SourceQueueWithComplete[NodeStreamMessage],
    peerMessageSenderApi: PeerMessageSenderApi
)(implicit
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig) {

  private val initPeerMessageRecv = PeerMessageReceiver(
    controlMessageHandler = controlMessageHandler,
    queue = queue,
    peer = peer,
    state = PeerMessageReceiverState.fresh())

  val peerMessageSender: PeerMessageSender = {
    PeerMessageSender(peer, initPeerMessageRecv, peerMessageSenderApi)
  }

  def stop(): Future[Unit] = {
    peerMessageSender.disconnect()
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

  private var _invalidMessagesCount: Int = 0

  def updateInvalidMessageCount(): Unit = {
    _invalidMessagesCount += 1
  }

  def getInvalidMessageCount = _invalidMessagesCount

  private var lastTimedOut: Long = 0

  def updateLastFailureTime(): Unit = {
    lastTimedOut = System.currentTimeMillis()
  }

  @volatile private[this] var lastSuccessfulParsedMsg: Long = 0

  def updateLastParsedMessageTime(): Unit = {
    lastSuccessfulParsedMsg = System.currentTimeMillis()
    ()
  }

  private val TIMEOUT_INTERVAL = 20.minute

  def isConnectionTimedOut: Boolean = {
    val timeoutInstant =
      Instant.now().minus(TIMEOUT_INTERVAL.toMillis, ChronoUnit.MILLIS)
    val diff = Instant
      .ofEpochMilli(lastSuccessfulParsedMsg)
      .minus(timeoutInstant.toEpochMilli, ChronoUnit.MILLIS)

    val isTimedout = diff.toEpochMilli < 0
    isTimedout
  }

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
