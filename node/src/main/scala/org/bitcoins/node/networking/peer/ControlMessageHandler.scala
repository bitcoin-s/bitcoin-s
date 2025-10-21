package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer.ControlMessageHandler.ControlMessageHandlerState
import org.bitcoins.node.util.PeerMessageSenderApi
import org.bitcoins.node.{P2PLogger, PeerFinder}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class ControlMessageHandler(peerFinder: PeerFinder)(implicit
    ec: ExecutionContext,
    nodeAppConfig: NodeAppConfig
) extends P2PLogger {

  def handleControlPayload(
      payload: ControlPayload,
      peerMsgSenderApi: PeerMessageSenderApi
  ): Future[Option[ControlMessageHandlerState]] = {
    val peer = peerMsgSenderApi.peer
    logger.debug(
      s"Received control message=${payload.commandName} from peer=$peer")
    payload match {
      case versionMsg: VersionMessage =>
        peerFinder.onVersionMessage(peer, versionMsg) match {
          case Some(_) =>
            peerMsgSenderApi
              .sendVerackMessage()
              .map(_ => None)
          case None =>
            logger.warn(s"Could not find peer=$peer in PeerFinder!")
            Future.successful(None)
        }

      case VerAckMessage =>
        val i = ControlMessageHandler.Initialized(peer)
        Future.successful(Some(i))
      case ping: PingMessage =>
        peerMsgSenderApi
          .sendPong(ping)
          .map(_ => None)
      case SendHeadersMessage =>
        // we want peers to just send us headers
        // we don't want to have to request them manually
        peerMsgSenderApi
          .sendHeadersMessage()
          .map(_ => None)
      case msg: GossipAddrMessage =>
        handleGossipAddrMessage(msg)
        Future.successful(Some(ControlMessageHandler.ReceivedAddrMessage))
      case SendAddrV2Message =>
        peerMsgSenderApi
          .sendSendAddrV2Message()
          .map(_ => None)
      case _ @(_: FilterAddMessage | _: FilterLoadMessage |
          FilterClearMessage) =>
        Future.successful(None)
      case _ @(GetAddrMessage | _: PongMessage) =>
        Future.successful(None)
      case _: RejectMessage =>
        Future.successful(None)
      case _: FeeFilterMessage =>
        Future.successful(None)
    }
  }

  private def handleGossipAddrMessage(message: GossipAddrMessage): Unit = {
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
          val peer = Peer.fromSocket(
            socket = inetAddress,
            socks5ProxyParams = nodeAppConfig.socks5ProxyParams
          )
          val pd = peerFinder.buildPeerData(peer, isPersistent = false)
          peerFinder.addToTry(Vector(pd), 0)
        }
      case addr: AddrV2Message =>
        val bytes = addr.bytes
        val port = addr.port.toInt
        val services = ServiceIdentifier.fromBytes(addr.services.bytes)
        val inetAddress =
          NetworkUtil.parseInetSocketAddress(bytes, port)
        val peer = Peer.fromSocket(
          socket = inetAddress,
          socks5ProxyParams = nodeAppConfig.socks5ProxyParams
        )
        val priority = if (services.nodeCompactFilters) 1 else 0
        addr match {
          case IPv4AddrV2Message(_, _, _, _) | IPv6AddrV2Message(_, _, _, _) =>
            val pd = peerFinder.buildPeerData(peer, isPersistent = false)
            peerFinder.addToTry(Vector(pd), priority = priority)
          case TorV3AddrV2Message(_, _, _, _) =>
            if (nodeAppConfig.torConf.enabled) {
              val pd = peerFinder.buildPeerData(peer, isPersistent = false)
              peerFinder.addToTry(Vector(pd), priority)
            }
          case n => logger.info(s"Unsupported network. Skipping. network=$n")
        }
    }
  }
}

object ControlMessageHandler {
  sealed trait ControlMessageHandlerState
  case class Initialized(peer: Peer) extends ControlMessageHandlerState

  case object ReceivedAddrMessage extends ControlMessageHandlerState
}
