package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.node.{
  ExternalImplementationNodeType,
  InternalImplementationNodeType,
  NodeType
}
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiverState._
import org.bitcoins.node.{Node, P2PLogger}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

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

            val newRecv = peerMessageReceiver.toState(newState)

            //
            if (node.peerManager.testPeerData.contains(peer)) {
              node.peerManager
                .testPeerData(peer)
                .setServiceIdentifier(versionMsg.services)
              for {
                _ <- sender.sendVerackMessage()
                _ <- onPeerInitialization(peer)
              } yield newRecv
            } else {
              Future(newRecv)
            }
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
      case msg: GossipAddrMessage =>
        handleGossipAddrMessage(msg)
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

  def handleGossipAddrMessage(message: GossipAddrMessage): Unit = {
    message match {
      case addr: AddrMessage =>
        addr.addresses.foreach { networkAddress =>
          if (networkAddress.services.nodeCompactFilters) {
            val bytes = Try(networkAddress.address.ipv4Bytes) match {
              case Failure(_) =>
                networkAddress.address.bytes
              case Success(ipv4Bytes) =>
                ipv4Bytes
            }
            val inetAddress =
              NetworkUtil.parseInetSocketAddress(bytes, networkAddress.port)
            val peer = Peer.fromSocket(socket = inetAddress,
                                       socks5ProxyParams =
                                         node.nodeAppConfig.socks5ProxyParams)
            node.peerManager.peerDiscoveryStack.push(peer)
          }
        }
      case addr: AddrV2Message =>
        val bytes = addr.bytes
        val port = addr.port.toInt
        val services = ServiceIdentifier.fromBytes(addr.services.bytes)
        val inetAddress =
          NetworkUtil.parseInetSocketAddress(bytes, port)
        val peer = Peer.fromSocket(socket = inetAddress,
                                   socks5ProxyParams =
                                     node.nodeAppConfig.socks5ProxyParams)
        addr match {
          case IPv4AddrV2Message(_, _, _, _) | IPv6AddrV2Message(_, _, _, _) =>
            if (services.nodeCompactFilters)
              node.peerManager.peerDiscoveryStack.push(peer)
          case TorV3AddrV2Message(_, _, _, _) =>
            if (
              node.nodeAppConfig.torConf.enabled && services.nodeCompactFilters
            )
              node.peerManager.peerDiscoveryStack.push(peer)
          case _ => logger.debug(s"Unsupported network. Skipping.")
        }
    }
  }

  def onPeerInitialization(peer: Peer): Future[Unit] = {
    node.nodeAppConfig.nodeType match {
      case nodeType: InternalImplementationNodeType =>
        nodeType match {
          case NodeType.FullNode =>
            throw new Exception("Node cannot be FullNode")
          case NodeType.NeutrinoNode =>
            if (
              node.peerManager
                .peerDataOf(peer)
                .serviceIdentifier
                .nodeCompactFilters
            ) {
              val createInDbF = node.peerManager.createInDb(peer)
              val managePeerF =
                if (
                  node.peerManager.connectedPeerCount < node.nodeAppConfig.maxConnectedPeers
                ) {
                  node.peerManager.setPeerForUse(peer)
                } else {
                  node.peerManager.removeTestPeer(peer)
                }
              for {
                _ <- createInDbF
                _ <- managePeerF
              } yield ()
            } else {
              node.peerManager.removeTestPeer(peer)
            }
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
