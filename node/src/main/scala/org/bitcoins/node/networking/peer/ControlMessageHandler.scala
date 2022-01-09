package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.node.{
  ExternalImplementationNodeType,
  InternalImplementationNodeType,
  NodeType
}
import org.bitcoins.core.p2p._
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiverState._
import org.bitcoins.node.{Node, P2PLogger}

import scala.concurrent.{ExecutionContext, Future}

case class ControlMessageHandler(node: Node)(implicit ec: ExecutionContext)
    extends P2PLogger {

  def handleControlPayload(
      payload: ControlPayload,
      sender: PeerMessageSender,
      peer: Peer,
      peerMessageReceiver: PeerMessageReceiver): Future[PeerMessageReceiver] = {
    val state = peerMessageReceiver.state
    payload match {

      case versionMsg: VersionMessage =>
        logger.trace(s"Received versionMsg=$versionMsg from peer=$peer")

        state match {
          case bad @ (_: Disconnected | _: Normal | Preconnection |
              _: InitializedDisconnect | _: InitializedDisconnectDone) =>
            Future.failed(
              new RuntimeException(
                s"Cannot handle version message while in state=${bad}"))

          case good: Initializing =>
            val newState = good.withVersionMsg(versionMsg)

            node.peerManager
              .peerData(peer)
              .setServiceIdentifier(versionMsg.services)

            val newRecv = peerMessageReceiver.toState(newState)

            for {
              _ <- sender.sendVerackMessage()
              _ <- onPeerInitialization(peer)
            } yield newRecv
        }

      case VerAckMessage =>
        state match {
          case bad @ (_: Disconnected | _: InitializedDisconnect | _: Normal |
              _: InitializedDisconnectDone | Preconnection) =>
            Future.failed(
              new RuntimeException(
                s"Cannot handle version message while in state=${bad}"))

          case good: Initializing =>
            val newState = good.toNormal(VerAckMessage)
            val newRecv = peerMessageReceiver.toState(newState)
            Future.successful(newRecv)
        }

      case ping: PingMessage =>
        sender.sendPong(ping).map(_ => peerMessageReceiver)
      case SendHeadersMessage =>
        //we want peers to just send us headers
        //we don't want to have to request them manually
        sender.sendHeadersMessage().map(_ => peerMessageReceiver)
      case _: GossipAddrMessage =>
        Future.successful(peerMessageReceiver)
      case SendAddrV2Message =>
        sender.sendSendAddrV2Message().map(_ => peerMessageReceiver)
      case _ @(_: FilterAddMessage | _: FilterLoadMessage |
          FilterClearMessage) =>
        Future.successful(peerMessageReceiver)
      case _ @(GetAddrMessage | _: PongMessage) =>
        Future.successful(peerMessageReceiver)
      case _: RejectMessage =>
        Future.successful(peerMessageReceiver)
      case _: FeeFilterMessage =>
        Future.successful(peerMessageReceiver)
    }
  }

  def onPeerInitialization(peer: Peer): Future[Unit] = {
    node.nodeAppConfig.nodeType match {
      case nodeType: InternalImplementationNodeType =>
        nodeType match {
          case NodeType.FullNode =>
            throw new Exception("Node cannot be FullNode")
          case NodeType.NeutrinoNode =>
            node.peerManager.createInDb(peer).map(_ => ())
          case NodeType.SpvNode =>
            node.peerManager.createInDb(peer).map(_ => ())
        }
      case nodeType: ExternalImplementationNodeType =>
        nodeType match {
          case NodeType.BitcoindBackend =>
            throw new Exception("Node cannot be BitcoindBackend")
        }
    }
  }
}
