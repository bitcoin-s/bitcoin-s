package org.bitcoins.node

import org.bitcoins.core.p2p.{
  DataPayload,
  ExpectsResponse,
  NetworkMessage,
  NetworkPayload
}
import org.bitcoins.node.models.Peer

sealed abstract class NodeStreamMessage

object NodeStreamMessage {

  case class DataMessageWrapper(payload: DataPayload, peer: Peer)
      extends NodeStreamMessage

  case class HeaderTimeoutWrapper(peer: Peer) extends NodeStreamMessage

  case class DisconnectedPeer(peer: Peer, forceReconnect: Boolean)
      extends NodeStreamMessage

  case class Initialized(peer: Peer) extends NodeStreamMessage

  case class InitializationTimeout(peer: Peer) extends NodeStreamMessage

  case class QueryTimeout(peer: Peer, payload: ExpectsResponse)
      extends NodeStreamMessage

  case class SendResponseTimeout(peer: Peer, payload: NetworkPayload)
      extends NodeStreamMessage

  case class SendToPeer(msg: NetworkMessage, peerOpt: Option[Peer])
}
