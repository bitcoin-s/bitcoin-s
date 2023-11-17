package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.NodeStreamMessage.Initialized
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.util.PeerMessageSenderApi
import org.bitcoins.node.{NodeStreamMessage, P2PLogger, PeerFinder}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class ControlMessageHandler(peerFinder: PeerFinder)(implicit
    ec: ExecutionContext,
    nodeAppConfig: NodeAppConfig)
    extends P2PLogger {

  def handleControlPayload(
      payload: ControlPayload,
      peerMsgSenderApi: PeerMessageSenderApi): Future[Option[Initialized]] = {
    val peer = peerMsgSenderApi.peer
    payload match {

      case versionMsg: VersionMessage =>
        logger.trace(s"Received versionMsg=$versionMsg from peer=$peer")
        peerFinder.onVersionMessage(peer, versionMsg)
        peerMsgSenderApi
          .sendVerackMessage()
          .map(_ => None)

      case VerAckMessage =>
        val i = NodeStreamMessage.Initialized(peer)
        Future.successful(Some(i))
      case ping: PingMessage =>
        peerMsgSenderApi
          .sendPong(ping)
          .map(_ => None)
      case SendHeadersMessage =>
        //we want peers to just send us headers
        //we don't want to have to request them manually
        peerMsgSenderApi
          .sendHeadersMessage()
          .map(_ => None)
      case msg: GossipAddrMessage =>
        handleGossipAddrMessage(msg)
        Future.successful(None)
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
          val peer = Peer.fromSocket(socket = inetAddress,
                                     socks5ProxyParams =
                                       nodeAppConfig.socks5ProxyParams)
          peerFinder.addToTry(Vector(peer), 0)
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
            peerFinder.addToTry(Vector(peer), priority = priority)
          case TorV3AddrV2Message(_, _, _, _) =>
            if (nodeAppConfig.torConf.enabled)
              peerFinder.addToTry(Vector(peer), priority)
          case n => logger.info(s"Unsupported network. Skipping. network=$n")
        }
    }
  }
}
