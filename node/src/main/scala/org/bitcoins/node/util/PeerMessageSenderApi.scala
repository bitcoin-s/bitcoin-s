package org.bitcoins.node.util

import org.bitcoins.core.api.chain.{ChainApi, FilterSyncMarker}
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p.{
  GetAddrMessage,
  InetAddress,
  NetworkPayload,
  PingMessage,
  PongMessage,
  SendAddrV2Message,
  SendHeadersMessage,
  TransactionMessage,
  TypeIdentifier,
  VerAckMessage,
  VersionMessage
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.constant.NodeConstants
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.NodeState

import scala.concurrent.{ExecutionContext, Future}

trait PeerMessageSenderApi {

  def sendGetDataMessage(
      typeIdentifier: TypeIdentifier,
      hash: DoubleSha256DigestBE,
      peerOpt: Option[Peer]): Future[Unit] = {
    sendGetDataMessages(typeIdentifier, Vector(hash), peerOpt)
  }

  def sendGetDataMessages(
      typeIdentifier: TypeIdentifier,
      hashes: Vector[DoubleSha256DigestBE],
      peerOpt: Option[Peer]): Future[Unit]

  def sendGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE],
      peerOpt: Option[Peer]): Future[Unit]

  /** Gossips the [[org.bitcoins.core.p2p.GetHeadersMessage]] to all of our peers to attempt ot get the best block headers */
  def gossipGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE]): Future[Unit]

  def sendGetHeadersMessage(
      lastHash: DoubleSha256DigestBE,
      peerOpt: Option[Peer]): Future[Unit] = {
    sendGetHeadersMessage(Vector(lastHash), peerOpt)
  }

  /** Gossips the given message to all peers except the excluded peer. If None given as excluded peer, gossip message to all peers */
  def gossipMessage(
      msg: NetworkPayload,
      excludedPeerOpt: Option[Peer]): Future[Unit]

  def sendMsg(msg: NetworkPayload, peerOpt: Option[Peer]): Future[Unit]

  def sendGetCompactFilterHeadersMessage(
      filterSyncMarker: FilterSyncMarker,
      peerOpt: Option[Peer]): Future[Unit]

  def sendGetCompactFiltersMessage(
      filterSyncMarker: FilterSyncMarker,
      peer: Peer)(implicit ec: ExecutionContext): Future[NodeState.FilterSync]

  def sendInventoryMessage(
      transactions: Vector[Transaction],
      peerOpt: Option[Peer]): Future[Unit]

  def sendSendAddrV2Message(peer: Peer): Future[Unit] = {
    sendMsg(SendAddrV2Message, Some(peer))
  }

  def sendGetAddrMessage(peerOpt: Option[Peer]): Future[Unit] = {
    sendMsg(GetAddrMessage, peerOpt)
  }

  /** Responds to a ping message */
  def sendPong(ping: PingMessage, peer: Peer): Future[Unit] = {
    val pong = PongMessage(ping.nonce)
    sendMsg(pong, Some(peer))
  }

  def sendHeadersMessage(peer: Peer): Future[Unit] = {
    val sendHeadersMsg = SendHeadersMessage
    sendMsg(sendHeadersMsg, Some(peer))
  }

  /** Sends a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] to our peer */
  def sendVersionMessage(peer: Peer)(implicit
      conf: NodeAppConfig): Future[Unit] = {
    val local = java.net.InetAddress.getLocalHost
    val versionMsg = VersionMessage(
      conf.network,
      InetAddress(peer.socket.getAddress.getAddress),
      InetAddress(local.getAddress),
      relay = conf.relay)
    sendMsg(versionMsg, Some(peer))
  }

  def sendVersionMessage(chainApi: ChainApi, peer: Peer)(implicit
      ec: ExecutionContext,
      conf: NodeAppConfig): Future[Unit] = {
    chainApi.getBestHashBlockHeight().flatMap { height =>
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg =
        VersionMessage(conf.network,
                       NodeConstants.userAgent,
                       Int32(height),
                       InetAddress(localhost.getAddress),
                       InetAddress(localhost.getAddress),
                       conf.relay)
      sendMsg(versionMsg, Some(peer))
    }
  }

  def sendVerackMessage(peer: Peer): Future[Unit] = {
    val verackMsg = VerAckMessage
    sendMsg(verackMsg, Some(peer))
  }

  def sendTransactionMessage(
      transaction: Transaction,
      peerOpt: Option[Peer]): Future[Unit] = {
    val message = TransactionMessage(transaction)
    sendMsg(message, peerOpt)
  }
}
