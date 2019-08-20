package org.bitcoins.node.networking.peer

import akka.actor.ActorRef
import akka.io.Tcp
import akka.util.Timeout
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.p2p.NetworkMessage
import org.bitcoins.core.p2p._
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.node.P2PLogger
import org.bitcoins.core.crypto.HashDigest
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.FutureUtil

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

case class PeerMessageSender(client: P2PClient)(implicit conf: NodeAppConfig)
    extends P2PLogger {
  private val socket = client.peer.socket
  implicit private val timeout = Timeout(10.seconds)

  /** Initiates a connection with the given peer */
  def connect(): Unit = {
    logger.info(s"Attempting to connect to peer=$socket")
    (client.actor ! Tcp.Connect(socket))
  }

  def isConnected()(implicit ec: ExecutionContext): Future[Boolean] = {
    client.isConnected()
  }

  def isInitialized()(implicit ec: ExecutionContext): Future[Boolean] = {
    client.isInitialized()
  }

  def isDisconnected()(implicit ec: ExecutionContext): Future[Boolean] = {
    client.isDisconnected()
  }

  /** Disconnects the given peer */
  def disconnect()(implicit ec: ExecutionContext): Future[Unit] = {
    isConnected().flatMap {
      case true =>
        logger.info(s"Disconnecting peer at socket=${socket}")
        (client.actor ! Tcp.Close)
        FutureUtil.unit
      case false =>
        val err =
          s"Cannot disconnect client that is not connected to socket=${socket}!"
        logger.warn(err)
        FutureUtil.unit
    }

  }

  /** Sends a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] to our peer */
  def sendVersionMessage()(implicit ec: ExecutionContext): Future[Unit] = {
    val versionMsg = VersionMessage(client.peer.socket, conf.network)
    logger.trace(s"Sending versionMsg=$versionMsg to peer=${client.peer}")
    sendMsg(versionMsg)
  }

  def sendVerackMessage()(implicit ec: ExecutionContext): Future[Unit] = {
    val verackMsg = VerAckMessage
    sendMsg(verackMsg)
  }

  /** Responds to a ping message */
  def sendPong(ping: PingMessage)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val pong = PongMessage(ping.nonce)
    logger.trace(s"Sending pong=$pong to peer=${client.peer}")
    sendMsg(pong)
  }

  def sendGetHeadersMessage(lastHash: DoubleSha256Digest)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val headersMsg = GetHeadersMessage(lastHash)
    logger.trace(s"Sending getheaders=$headersMsg to peer=${client.peer}")
    sendMsg(headersMsg)
  }

  def sendHeadersMessage()(implicit ec: ExecutionContext): Future[Unit] = {
    val sendHeadersMsg = SendHeadersMessage
    sendMsg(sendHeadersMsg)
  }

  /**
    * Sends a inventory message with the given transactions
    */
  def sendInventoryMessage(transactions: Transaction*)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val inventories =
      transactions.map(tx => Inventory(TypeIdentifier.MsgTx, tx.txId))
    val message = InventoryMessage(inventories)
    logger.trace(s"Sending inv=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendFilterClearMessage()(implicit ec: ExecutionContext): Future[Unit] = {
    sendMsg(FilterClearMessage)
  }

  def sendFilterAddMessage(hash: HashDigest)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val message = FilterAddMessage.fromHash(hash)
    logger.trace(s"Sending filteradd=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendFilterLoadMessage(bloom: BloomFilter)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val message = FilterLoadMessage(bloom)
    logger.trace(s"Sending filterload=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendTransactionMessage(transaction: Transaction)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val message = TransactionMessage(transaction)
    logger.trace(s"Sending txmessage=$message to peer=${client.peer}")
    sendMsg(message)
  }

  /** Sends a request for filtered blocks matching the given headers */
  def sendGetDataMessage(headers: BlockHeader*)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val inventories =
      headers.map(header =>
        Inventory(TypeIdentifier.MsgFilteredBlock, header.hash))
    val message = GetDataMessage(inventories)
    logger.info(s"Sending getdata=$message to peer=${client.peer}")
    sendMsg(message)
  }

  private[node] def sendMsg(msg: NetworkPayload)(
      implicit ec: ExecutionContext): Future[Unit] = {
    isInitialized().map { _ =>
      logger.debug(s"Sending msg=${msg.commandName} to peer=${socket}")
      val newtworkMsg = NetworkMessage(conf.network, msg)
      client.actor ! newtworkMsg
    }
  }
}

object PeerMessageSender {

  sealed abstract class PeerMessageHandlerMsg

  /**
    * For when we are done with exchanging version and verack messages
    * This means we can send normal p2p messages now
    */
  case object HandshakeFinished extends PeerMessageHandlerMsg

  case class SendToPeer(msg: NetworkMessage) extends PeerMessageHandlerMsg

  /** Accumulators network messages while we are doing a handshake with our peer
    * and caches a peer handler actor so we can send a [[HandshakeFinished]]
    * message back to the actor when we are fully connected
    */
  case class MessageAccumulator(
      networkMsgs: Vector[(ActorRef, NetworkMessage)],
      peerHandler: ActorRef)

}
