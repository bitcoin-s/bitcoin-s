package org.bitcoins.node.networking.peer

import org.bitcoins.core.p2p._
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.PeerMessageReceiverState._
import org.bitcoins.node.{P2PLogger, PeerManager}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class ControlMessageHandler(peerManager: PeerManager)(implicit
    ec: ExecutionContext,
    nodeAppConfig: NodeAppConfig)
    extends P2PLogger {

  def handleControlPayload(
      payload: ControlPayload,
      sender: PeerMessageSender,
      peer: Peer,
      state: PeerMessageReceiverState): Future[PeerMessageReceiverState] = {
    payload match {

      case versionMsg: VersionMessage =>
        logger.trace(s"Received versionMsg=$versionMsg from peer=$peer")

        state match {
          case bad @ (_: Disconnected | _: Normal | Preconnection |
              _: InitializedDisconnect | _: InitializedDisconnectDone |
              _: StoppedReconnect | _: Waiting) =>
            Future.failed(
              new RuntimeException(
                s"Cannot handle version message while in state=${bad}"))

          case good: Initializing =>
            val newState = good.withVersionMsg(versionMsg)

            peerManager.onVersionMessage(peer, versionMsg)

            sender.sendVerackMessage().map(_ => newState)
        }

      case VerAckMessage =>
        state match {
          case bad @ (_: Disconnected | _: InitializedDisconnect | _: Normal |
              _: InitializedDisconnectDone | Preconnection |
              _: StoppedReconnect | _: Waiting) =>
            Future.failed(
              new RuntimeException(
                s"Cannot handle version message while in state=${bad}"))

          case good: Initializing =>
            val newState = good.toNormal(VerAckMessage)

            peerManager.onInitialization(peer).map(_ => newState)
        }

      case ping: PingMessage =>
        sender.sendPong(ping).map { _ =>
          state
        }
      case SendHeadersMessage =>
        //we want peers to just send us headers
        //we don't want to have to request them manually
        sender.sendHeadersMessage().map(_ => state)
      case msg: GossipAddrMessage =>
        handleGossipAddrMessage(msg)
        Future.successful(state)
      case SendAddrV2Message =>
        sender.sendSendAddrV2Message().map(_ => state)
      case _ @(_: FilterAddMessage | _: FilterLoadMessage |
          FilterClearMessage) =>
        Future.successful(state)
      case _ @(GetAddrMessage | _: PongMessage) =>
        Future.successful(state)
      case _: RejectMessage =>
        Future.successful(state)
      case _: FeeFilterMessage =>
        Future.successful(state)
    }
  }

  def handleGossipAddrMessage(message: GossipAddrMessage): Unit = {
    message match {
      case addr: AddrMessage =>
        addr.addresses.foreach { networkAddress =>
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
                                       nodeAppConfig.socks5ProxyParams)
          peerManager.addPeerToTry(Vector(peer), 0)
        }
      case addr: AddrV2Message =>
        val bytes = addr.bytes
        val port = addr.port.toInt
        val services = ServiceIdentifier.fromBytes(addr.services.bytes)
        val inetAddress =
          NetworkUtil.parseInetSocketAddress(bytes, port)
        val peer = Peer.fromSocket(socket = inetAddress,
                                   socks5ProxyParams =
                                     nodeAppConfig.socks5ProxyParams)
        val priority = if (services.nodeCompactFilters) 1 else 0
        addr match {
          case IPv4AddrV2Message(_, _, _, _) | IPv6AddrV2Message(_, _, _, _) =>
            peerManager.addPeerToTry(Vector(peer), priority = priority)
          case TorV3AddrV2Message(_, _, _, _) =>
            if (nodeAppConfig.torConf.enabled)
              peerManager.addPeerToTry(Vector(peer), priority)
          case _ => logger.debug(s"Unsupported network. Skipping.")
        }
    }
  }
}
