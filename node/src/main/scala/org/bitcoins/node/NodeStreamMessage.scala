package org.bitcoins.node

import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p.*

import java.time.Instant

sealed abstract class NodeStreamMessage

object NodeStreamMessage {

  case class DataMessageWrapper(
      payload: DataPayload,
      peer: Peer,
      receivedAt: Instant = Instant.now())
      extends NodeStreamMessage

  case class ControlMessageWrapper(payload: ControlPayload, peer: Peer)
      extends NodeStreamMessage

  case class HeaderTimeoutWrapper(peer: Peer) extends NodeStreamMessage

  case class ConnectPeer(peer: Peer) extends NodeStreamMessage

  case class InitializeDisconnect(peer: Peer) extends NodeStreamMessage

  case class DisconnectedPeer(peer: Peer, forceReconnect: Boolean)
      extends NodeStreamMessage

  case class InitializationTimeout(peer: Peer) extends NodeStreamMessage

  case class QueryTimeout(peer: Peer, payload: ExpectsResponse)
      extends NodeStreamMessage

  case class SendResponseTimeout(peer: Peer, payload: NetworkPayload)
      extends NodeStreamMessage

  case class StartSync(peerOpt: Option[Peer]) extends NodeStreamMessage

  case class SendToPeer(msg: NetworkMessage, peerOpt: Option[Peer])
      extends NodeStreamMessage

  case class GossipMessage(msg: NetworkMessage, excludePeerOpt: Option[Peer])
      extends NodeStreamMessage

  case object NodeShutdown extends NodeStreamMessage

  /** Checks our peers are healthy, for instance checking that we are peered
    * with compact filter peers
    */
  case object PeerHealthCheck extends NodeStreamMessage
}
