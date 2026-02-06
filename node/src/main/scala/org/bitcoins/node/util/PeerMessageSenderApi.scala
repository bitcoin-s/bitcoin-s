package org.bitcoins.node.util

import org.apache.pekko.stream.QueueOfferResult
import org.bitcoins.core.api.chain.{ChainApi, FilterSyncMarker}
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.api.node.constant.NodeConstants
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

import scala.concurrent.{ExecutionContext, Future}

trait PeerMessageSenderApi {

  def peer: Peer

  def sendGetDataMessage(
      typeIdentifier: TypeIdentifier,
      hash: DoubleSha256DigestBE
  ): Future[QueueOfferResult] = {
    sendGetDataMessages(typeIdentifier, Vector(hash))
  }

  def sendGetDataMessages(
      typeIdentifier: TypeIdentifier,
      hashes: Vector[DoubleSha256DigestBE]
  ): Future[QueueOfferResult]

  def sendGetHeadersMessage(
      hashes: Vector[DoubleSha256DigestBE]): Future[QueueOfferResult]

  def sendGetHeadersMessage(
      lastHash: DoubleSha256DigestBE): Future[QueueOfferResult] = {
    sendGetHeadersMessage(Vector(lastHash))
  }

  def sendMsg(msg: NetworkPayload): Future[QueueOfferResult]

  def sendGetCompactFilterHeadersMessage(
      filterSyncMarker: FilterSyncMarker
  ): Future[QueueOfferResult]

  def sendGetCompactFiltersMessage(
      filterSyncMarker: FilterSyncMarker): Future[QueueOfferResult]

  def sendInventoryMessage(
      transactions: Vector[Transaction]): Future[QueueOfferResult]

  def sendSendAddrV2Message(): Future[QueueOfferResult] = {
    sendMsg(SendAddrV2Message)
  }

  def sendGetAddrMessage(): Future[QueueOfferResult] = {
    sendMsg(GetAddrMessage)
  }

  /** Responds to a ping message */
  def sendPong(ping: PingMessage): Future[QueueOfferResult] = {
    val pong = PongMessage(ping.nonce)
    sendMsg(pong)
  }

  def sendHeadersMessage(): Future[QueueOfferResult] = {
    val sendHeadersMsg = SendHeadersMessage
    sendMsg(sendHeadersMsg)
  }

  /** Sends a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] to our
    * peer
    */
  def sendVersionMessage()(implicit
      conf: NodeAppConfig): Future[QueueOfferResult]

  def sendVersionMessage(
      chainApi: ChainApi
  )(implicit
      ec: ExecutionContext,
      conf: NodeAppConfig): Future[QueueOfferResult] = {
    chainApi.getBestHashBlockHeight().flatMap { height =>
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg =
        VersionMessage(
          conf.network,
          NodeConstants.userAgent,
          Int32(height),
          InetAddress(localhost.getAddress),
          InetAddress(localhost.getAddress),
          conf.relay
        )
      sendMsg(versionMsg)
    }
  }

  def sendVerackMessage(): Future[QueueOfferResult] = {
    val verackMsg = VerAckMessage
    sendMsg(verackMsg)
  }

  def sendTransactionMessage(
      transaction: Transaction): Future[QueueOfferResult] = {
    val message = TransactionMessage(transaction)
    sendMsg(message)
  }
}
