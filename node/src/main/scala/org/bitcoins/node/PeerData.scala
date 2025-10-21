package org.bitcoins.node

import org.apache.pekko.Done
import org.apache.pekko.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.{Peer, PeerWithServices}
import org.bitcoins.core.p2p.{ServiceIdentifier, VersionMessage}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer.*

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/** PeerData contains objects specific to a peer associated together
  */
sealed trait PeerData {

  implicit protected def nodeAppConfig: NodeAppConfig
  implicit protected def chainAppConfig: ChainAppConfig

  implicit protected def system: ActorSystem
  def peer: Peer

  def peerWithServicesOpt: Option[PeerWithServices] = {
    _versionMessage.map(v => PeerWithServices(peer, v.services))
  }

  def peerMessageSender: PeerMessageSender

  def disconnect(): Future[Done] = {
    peerConnection.disconnect()
  }

  def peerConnection: PeerConnection = {
    peerMessageSender.peerConnection
  }

  private var _versionMessage: Option[VersionMessage] = None

  def versionMessage: VersionMessage = _versionMessage.getOrElse {
    throw new RuntimeException(
      s"Tried using VersionMessage for uninitialized peer=$peer"
    )
  }
  def serviceIdentifier: ServiceIdentifier = {
    versionMessage.services
  }

  def setVersionMessage(versionMessage: VersionMessage): Unit = {
    _versionMessage = Some(versionMessage)
  }

  def userAgent: String = _versionMessage.map(_.userAgent).getOrElse {
    sys.error(s"Tried using user agent for uninitialized peer=$peer")
  }
}

/** A peer we plan on being connected to persistently */
case class PersistentPeerData(
    peer: Peer,
    peerMessageSender: PeerMessageSender
)(implicit
    override val system: ActorSystem,
    override val nodeAppConfig: NodeAppConfig,
    override val chainAppConfig: ChainAppConfig
) extends PeerData {

  private var _invalidMessagesCount: Int = 0

  def updateInvalidMessageCount(): Unit = {
    _invalidMessagesCount += 1
  }

  def getInvalidMessageCount = _invalidMessagesCount

  private var lastTimedOut: Long = 0

  def updateLastFailureTime(): Unit = {
    lastTimedOut = System.currentTimeMillis()
  }

  /** returns true if the peer has failed due to any reason within the past 30
    * minutes
    */
  def hasFailedRecently: Boolean = {
    val timePast = System.currentTimeMillis() - lastTimedOut
    timePast < 30.minutes.toMillis
  }

  def exceededMaxInvalidMessages: Boolean = {
    _invalidMessagesCount > nodeAppConfig.maxInvalidResponsesAllowed
  }
}

/** A peer we are just discovering on the p2p network for future connections we
  * do not want to be persistently connected to this peer, just see if we can
  * connect to it and exchange version/verack messages
  */
case class AttemptToConnectPeerData(
    peer: Peer,
    peerMessageSender: PeerMessageSender
)(implicit
    override val system: ActorSystem,
    override val nodeAppConfig: NodeAppConfig,
    override val chainAppConfig: ChainAppConfig
) extends PeerData {

  def toPersistentPeerData: PersistentPeerData = {
    val p = PersistentPeerData(peer, peerMessageSender)
    p.setVersionMessage(versionMessage = versionMessage)
    p
  }
}
