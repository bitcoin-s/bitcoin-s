package org.bitcoins.node.util

import org.bitcoins.core.api.chain.{ChainApi, FilterSyncMarker}
import org.bitcoins.core.api.node.Peer
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

import scala.concurrent.{ExecutionContext, Future}

trait PeerMessageSenderApi {

  def peer: Peer

  def sendGetDataMessage(
      typeIdentifier: TypeIdentifier,
      hash: DoubleSha256DigestBE): Future[Unit] = {
    sendGetDataMessages(typeIdentifier, Vector(hash))
  }

  def sendGetDataMessages(
      typeIdentifier: TypeIdentifier,
      hashes: Vector[DoubleSha256DigestBE]): Future[Unit]

  def sendGetHeadersMessage(hashes: Vector[DoubleSha256DigestBE]): Future[Unit]

  def sendGetHeadersMessage(lastHash: DoubleSha256DigestBE): Future[Unit] = {
    sendGetHeadersMessage(Vector(lastHash))
  }

  def sendMsg(msg: NetworkPayload): Future[Unit]

  def sendGetCompactFilterHeadersMessage(
      filterSyncMarker: FilterSyncMarker): Future[Unit]

  def sendGetCompactFiltersMessage(filterSyncMarker: FilterSyncMarker)(implicit
      ec: ExecutionContext): Future[Unit]

  def sendInventoryMessage(transactions: Vector[Transaction]): Future[Unit]

  def sendSendAddrV2Message(): Future[Unit] = {
    sendMsg(SendAddrV2Message)
  }

  def sendGetAddrMessage(): Future[Unit] = {
    sendMsg(GetAddrMessage)
  }

  /** Responds to a ping message */
  def sendPong(ping: PingMessage): Future[Unit] = {
    val pong = PongMessage(ping.nonce)
    sendMsg(pong)
  }

  def sendHeadersMessage(): Future[Unit] = {
    val sendHeadersMsg = SendHeadersMessage
    sendMsg(sendHeadersMsg)
  }

  /** Sends a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] to our
    * peer
    */
  def sendVersionMessage()(implicit conf: NodeAppConfig): Future[Unit]

  def sendVersionMessage(chainApi: ChainApi)(implicit
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
      sendMsg(versionMsg)
    }
  }

  def sendVerackMessage(): Future[Unit] = {
    val verackMsg = VerAckMessage
    sendMsg(verackMsg)
  }

  def sendTransactionMessage(transaction: Transaction): Future[Unit] = {
    val message = TransactionMessage(transaction)
    sendMsg(message)
  }
}
