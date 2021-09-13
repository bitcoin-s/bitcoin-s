package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.node.NodeType.NeutrinoNode
import org.bitcoins.core.p2p.{
  AddrMessage,
  AddrV2Message,
  ControlPayload,
  FeeFilterMessage,
  FilterAddMessage,
  FilterClearMessage,
  FilterLoadMessage,
  GetAddrMessage,
  GossipAddrMessage,
  IPv4AddrV2Message,
  IPv6AddrV2Message,
  PingMessage,
  PongMessage,
  RejectMessage,
  SendAddrV2Message,
  SendHeadersMessage,
  ServiceIdentifier,
  TorV3AddrV2Message,
  VerAckMessage,
  VersionMessage
}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.{Node, P2PLogger}
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiverState._

import scala.concurrent.{ExecutionContext, Future}

case class ControlMessageHandler(node: Node)(implicit
    nodeAppConfig: NodeAppConfig,
    ec: ExecutionContext)
    extends P2PLogger {

  def handleControlPayload(
      payload: ControlPayload,
      sender: PeerMessageSender,
      peer: Peer,
      state: PeerMessageReceiverState,
      receiver: PeerMessageReceiver): Future[PeerMessageReceiver] = {
    payload match {
      case versionMsg: VersionMessage =>
        logger.trace(s"Received versionMsg=$versionMsg from peer=$peer")

        state match {
          case bad @ (_: Disconnected | _: Normal | Preconnection |
              _: InitializedDisconnect) =>
            Future.failed(
              new RuntimeException(
                s"Cannot handle version message while in state=$bad"))

          case good: Initializing =>
            val newState = good.withVersionMsg(versionMsg)

            node.peerData(peer).setServiceIdentifier(versionMsg.services)
            val newRecv = receiver.toState(newState)

            for {
              _ <- sender.sendVerackMessage()
              _ <- onPeerInitialization(peer)
            } yield newRecv
        }

      case VerAckMessage =>
        state match {
          case bad @ (_: Disconnected | _: InitializedDisconnect | _: Normal |
              Preconnection) =>
            Future.failed(
              new RuntimeException(
                s"Cannot handle version message while in state=$bad"))

          case good: Initializing =>
            val newState = good.toNormal(VerAckMessage)
            val newRecv = receiver.toState(newState)
            Future.successful(newRecv)
        }

      case ping: PingMessage =>
        sender.sendPong(ping).map(_ => receiver)
      case SendHeadersMessage =>
        //we want peers to just send us headers
        //we don't want to have to request them manually
        sender.sendHeadersMessage().map(_ => receiver)
      case msg: GossipAddrMessage =>
        handlePeerGossipMessage(msg)
        Future.successful(receiver)
      case SendAddrV2Message =>
        sender.sendSendAddrV2Message().map(_ => receiver)
      case _ @(_: FilterAddMessage | _: FilterLoadMessage |
          FilterClearMessage) =>
        Future.successful(receiver)
      case _ @(GetAddrMessage | _: PongMessage) =>
        Future.successful(receiver)
      case _: RejectMessage =>
        Future.successful(receiver)
      case _: FeeFilterMessage =>
        Future.successful(receiver)
    }
  }

  def onPeerInitialization(peer: Peer): Future[Unit] = {
    //if its not removed then it means it means initialization was successful as in failed initializations are removed
    //from peerData
    node.nodeAppConfig.nodeType match {
      case NeutrinoNode =>
        val peerData = node.peerData
        val createdInDbF = node.createInDb(peer)
        val handlePeerF =
          if (
            peerData(peer).keepConnection || node.connectedPeersCount < node.maxConnectedPeers
          ) {
            val sendMsgF = peerData(peer).peerMessageSender.sendGetAddrMessage()
            //only the nodes that have keepConnection as true would be actually used by us
            peerData(peer).setToKeepConnection()
            logger.info(
              s"Connected to peer $peer with compact filters. Connected peer count ${node.connectedPeersCount}")
            sendMsgF
          } else {
            node.removePeer(peer)
            Future.unit
          }
        for {
          _ <- createdInDbF
          _ <- handlePeerF
        } yield ()
      case _ => Future.unit
    }
  }

  def handlePeerGossipMessage(message: GossipAddrMessage): Unit = {
    message match {
      case addr: AddrMessage =>
        addr.addresses.foreach(node.addToPeerQueue)
      case addr: AddrV2Message =>
        val bytes = addr.bytes
        val port = addr.port.toInt
        val services = ServiceIdentifier.fromBytes(addr.services.bytes)
        val inetAddress =
          NetworkUtil.parseInetSocketAddress(bytes, port)
        val peer = Peer.fromSocket(socket = inetAddress,
                                   socks5ProxyParams =
                                     nodeAppConfig.socks5ProxyParams)
        addr match {
          case IPv4AddrV2Message(_, _, _, _) | IPv6AddrV2Message(_, _, _, _) =>
            if (services.nodeCompactFilters)
              node.peersToCheckStack.push(peer)
          case TorV3AddrV2Message(_, _, _, _) =>
            if (
              node.nodeAppConfig.torConf.enabled && services.nodeCompactFilters
            )
              node.peersToCheckStack.push(peer)
          case _ => logger.debug(s"Unsupported network. Skipping.")
        }
    }
  }
}
