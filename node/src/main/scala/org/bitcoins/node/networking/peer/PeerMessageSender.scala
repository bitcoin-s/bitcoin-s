package org.bitcoins.node.networking.peer

import org.apache.pekko.stream.QueueOfferResult
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
  NetworkPayload,
  TypeIdentifier,
  VersionMessage
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.util.PeerMessageSenderApi

import scala.concurrent.Future

case class PeerMessageSender(peerConnection: PeerConnection)
    extends PeerMessageSenderApi
    with P2PLogger {

  override val peer: Peer = peerConnection.peer

  override def sendMsg(msg: NetworkPayload): Future[QueueOfferResult] = {
    peerConnection.sendMsg(msg)
  }

  override def sendGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE]
  ): Future[QueueOfferResult] = {
    val headersMsg = GetHeadersMessage(hashes.distinct.take(101).map(_.flip))
    sendMsg(headersMsg)
  }

  override def sendGetDataMessages(
      typeIdentifier: TypeIdentifier,
      hashes: Vector[DoubleSha256DigestBE]
  ): Future[QueueOfferResult] = {
    val msg: NetworkPayload = {
      val inventories =
        hashes.map(hash => Inventory(typeIdentifier, hash.flip))
      val message = GetDataMessage(inventories)
      message
    }

    sendMsg(msg)
  }

  override def sendGetCompactFilterHeadersMessage(
      filterSyncMarker: FilterSyncMarker
  ): Future[QueueOfferResult] = {
    val message =
      GetCompactFilterHeadersMessage(
        if (filterSyncMarker.startHeight < 0) 0
        else filterSyncMarker.startHeight,
        filterSyncMarker.stopBlockHash
      )
    sendMsg(message)
  }

  override def sendGetCompactFiltersMessage(
      filterSyncMarker: FilterSyncMarker): Future[QueueOfferResult] = {
    val message =
      GetCompactFiltersMessage(
        if (filterSyncMarker.startHeight < 0) 0
        else filterSyncMarker.startHeight,
        filterSyncMarker.stopBlockHash
      )
    logger.debug(s"Sending getcfilters=$message to peer ${peer}")
    sendMsg(message)
  }

  override def sendInventoryMessage(
      transactions: Vector[Transaction]
  ): Future[QueueOfferResult] = {
    val inventories =
      transactions.map(tx => Inventory(TypeIdentifier.MsgTx, tx.txId))
    val message = InventoryMessage(inventories)
    sendMsg(message)

  }

  /** Sends a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] to our
    * peer
    */
  override def sendVersionMessage()(implicit
      conf: NodeAppConfig
  ): Future[QueueOfferResult] = {
    val local = java.net.InetAddress.getLocalHost
    val versionMsg = VersionMessage(
      conf.network,
      InetAddress(peer.socket.getAddress.getAddress),
      InetAddress(local.getAddress),
      relay = conf.relay
    )
    sendMsg(versionMsg)
  }
}
