package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p._
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.{NodeStreamMessage, P2PLogger, PeerManager}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class ControlMessageHandler(peerManager: PeerManager)(implicit
    ec: ExecutionContext,
    nodeAppConfig: NodeAppConfig)
    extends P2PLogger {

  def handleControlPayload(
      payload: ControlPayload,
      peer: Peer): Future[Unit] = {
    payload match {

      case versionMsg: VersionMessage =>
        logger.trace(s"Received versionMsg=$versionMsg from peer=$peer")
        peerManager.onVersionMessage(peer, versionMsg)
        peerManager.sendVerackMessage(peer)

      case VerAckMessage =>
        val i = NodeStreamMessage.Initialized(peer)
        peerManager.offer(i).map(_ => ())

      case ping: PingMessage =>
        peerManager.sendPong(ping, peer)
      case SendHeadersMessage =>
        //we want peers to just send us headers
        //we don't want to have to request them manually
        peerManager.sendHeadersMessage(peer)
      case msg: GossipAddrMessage =>
        handleGossipAddrMessage(msg)
        Future.unit
      case SendAddrV2Message =>
        peerManager.sendSendAddrV2Message(peer)
      case _ @(_: FilterAddMessage | _: FilterLoadMessage |
          FilterClearMessage) =>
        Future.unit
      case _ @(GetAddrMessage | _: PongMessage) =>
        Future.unit
      case _: RejectMessage =>
        Future.unit
      case _: FeeFilterMessage =>
        Future.unit
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
          case n => logger.info(s"Unsupported network. Skipping. network=$n")
        }
    }
  }
}
