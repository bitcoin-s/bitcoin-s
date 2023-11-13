package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.chain.FilterSyncMarker
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p.{
  GetCompactFilterHeadersMessage,
  GetCompactFiltersMessage,
  GetDataMessage,
  GetHeadersMessage,
  InetAddress,
  Inventory,
  InventoryMessage,
  NetworkMessage,
  NetworkPayload,
  TypeIdentifier,
  VersionMessage
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.NodeStreamMessage.SendToPeer
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.util.PeerMessageSenderApi

import scala.concurrent.{ExecutionContext, Future}

case class PeerMessageSender(peerConnection: PeerConnection)(implicit
    nodeAppConfig: NodeAppConfig)
    extends PeerMessageSenderApi
    with P2PLogger {

  override val peer: Peer = peerConnection.peer

  override def sendMsg(msg: NetworkPayload): Future[Unit] = {
    val networkMessage = NetworkMessage(nodeAppConfig.network, msg)

    val sendToPeer = SendToPeer(networkMessage, Some(peer))
    logger.debug(
      s"Sending message ${sendToPeer.msg.payload.commandName} to peerOpt=${sendToPeer.peerOpt}")

    peerConnection.sendMsg(msg)
  }

  override def sendGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE]): Future[Unit] = {
    val headersMsg = GetHeadersMessage(hashes.distinct.take(101).map(_.flip))
    sendMsg(headersMsg)
  }

  override def sendGetDataMessages(
      typeIdentifier: TypeIdentifier,
      hashes: Vector[DoubleSha256DigestBE]): Future[Unit] = {
    val msg: NetworkPayload = {
      val inventories =
        hashes.map(hash => Inventory(typeIdentifier, hash.flip))
      val message = GetDataMessage(inventories)
      message
    }

    sendMsg(msg)
  }

  override def sendGetCompactFilterHeadersMessage(
      filterSyncMarker: FilterSyncMarker): Future[Unit] = {
    val message =
      GetCompactFilterHeadersMessage(if (filterSyncMarker.startHeight < 0) 0
                                     else filterSyncMarker.startHeight,
                                     filterSyncMarker.stopBlockHash)
    sendMsg(message)
  }

  override def sendGetCompactFiltersMessage(filterSyncMarker: FilterSyncMarker)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val message =
      GetCompactFiltersMessage(if (filterSyncMarker.startHeight < 0) 0
                               else filterSyncMarker.startHeight,
                               filterSyncMarker.stopBlockHash)
    logger.debug(s"Sending getcfilters=$message to peer ${peer}")
    sendMsg(message).map(_ => ())(ec)
  }

  override def sendInventoryMessage(
      transactions: Vector[Transaction]): Future[Unit] = {
    val inventories =
      transactions.map(tx => Inventory(TypeIdentifier.MsgTx, tx.txId))
    val message = InventoryMessage(inventories)
    sendMsg(message)

  }

  /** Sends a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] to our peer */
  override def sendVersionMessage()(implicit
      conf: NodeAppConfig): Future[Unit] = {
    val local = java.net.InetAddress.getLocalHost
    val versionMsg = VersionMessage(
      conf.network,
      InetAddress(peer.socket.getAddress.getAddress),
      InetAddress(local.getAddress),
      relay = conf.relay)
    sendMsg(versionMsg)
  }
}
