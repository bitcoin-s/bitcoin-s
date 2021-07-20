package org.bitcoins.node.networking.peer

import akka.actor.ActorRef
import akka.io.Tcp
import akka.util.Timeout
import org.bitcoins.core.api.chain.{ChainApi, FilterSyncMarker}
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.number.Int32
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  HashDigest
}
import org.bitcoins.node.P2PLogger
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.P2PClient

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

case class PeerMessageSender(client: P2PClient)(implicit conf: NodeAppConfig)
    extends P2PLogger {
  private val socket = client.peer.socket
  implicit private val timeout = Timeout(30.seconds)

  /** Initiates a connection with the given peer */
  def connect(): Unit = {
    client.actor ! P2PClient.ConnectCommand
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
        Future.unit
      case false =>
        val err =
          s"Cannot disconnect client that is not connected to socket=${socket}!"
        logger.warn(err)
        Future.unit
    }

  }

  /** Sends a [[org.bitcoins.core.p2p.VersionMessage VersionMessage]] to our peer */
  def sendVersionMessage(): Future[Unit] = {
    val local = java.net.InetAddress.getLocalHost
    val versionMsg = VersionMessage(
      conf.network,
      InetAddress(client.peer.socket.getAddress.getAddress),
      InetAddress(local.getAddress),
      relay = conf.relay)
    logger.trace(s"Sending versionMsg=$versionMsg to peer=${client.peer}")
    sendMsg(versionMsg)
  }

  def sendVersionMessage(chainApi: ChainApi)(implicit
      ec: ExecutionContext): Future[Unit] = {
    chainApi.getBestHashBlockHeight().flatMap { height =>
      val localhost = java.net.InetAddress.getLocalHost
      val versionMsg =
        VersionMessage(conf.network,
                       "/Bitcoin-S:0.7.0/",
                       Int32(height),
                       InetAddress(localhost.getAddress),
                       InetAddress(localhost.getAddress),
                       conf.relay)

      logger.debug(s"Sending versionMsg=$versionMsg to peer=${client.peer}")
      sendMsg(versionMsg)
    }
  }

  def sendVerackMessage(): Future[Unit] = {
    val verackMsg = VerAckMessage
    sendMsg(verackMsg)
  }

  def sendSendAddrV2Message(): Future[Unit] = {
    sendMsg(SendAddrV2Message)
  }

  /** Responds to a ping message */
  def sendPong(ping: PingMessage): Future[Unit] = {
    val pong = PongMessage(ping.nonce)
    logger.trace(s"Sending pong=$pong to peer=${client.peer}")
    sendMsg(pong)
  }

  def sendGetHeadersMessage(lastHash: DoubleSha256Digest): Future[Unit] = {
    val headersMsg = GetHeadersMessage(lastHash)
    logger.trace(s"Sending getheaders=$headersMsg to peer=${client.peer}")
    sendMsg(headersMsg)
  }

  def sendGetHeadersMessage(
      hashes: Vector[DoubleSha256Digest]): Future[Unit] = {
    // GetHeadersMessage has a max of 101 hashes
    val headersMsg = GetHeadersMessage(hashes.distinct.take(101))
    logger.trace(s"Sending getheaders=$headersMsg to peer=${client.peer}")
    sendMsg(headersMsg)
  }

  def sendHeadersMessage(): Future[Unit] = {
    val sendHeadersMsg = SendHeadersMessage
    sendMsg(sendHeadersMsg)
  }

  /** Sends a inventory message with the given transactions
    */
  def sendInventoryMessage(transactions: Transaction*): Future[Unit] = {
    val inventories =
      transactions.map(tx => Inventory(TypeIdentifier.MsgTx, tx.txId))
    val message = InventoryMessage(inventories)
    logger.trace(s"Sending inv=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendFilterClearMessage(): Future[Unit] = {
    sendMsg(FilterClearMessage)
  }

  def sendFilterAddMessage(hash: HashDigest): Future[Unit] = {
    val message = FilterAddMessage.fromHash(hash)
    logger.trace(s"Sending filteradd=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendFilterLoadMessage(bloom: BloomFilter): Future[Unit] = {
    val message = FilterLoadMessage(bloom)
    logger.trace(s"Sending filterload=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendTransactionMessage(transaction: Transaction): Future[Unit] = {
    val message = TransactionMessage(transaction)
    logger.debug(s"Sending txmessage=$message to peer=${client.peer}")
    sendMsg(message)
  }

  /** Sends a request for filtered blocks matching the given headers */
  def sendGetDataMessage(
      typeIdentifier: TypeIdentifier,
      hashes: DoubleSha256Digest*): Future[Unit] = {
    val inventories =
      hashes.map(hash => Inventory(typeIdentifier, hash))
    val message = GetDataMessage(inventories)
    logger.debug(s"Sending getdata=$message to peer=${client.peer}")
    sendMsg(message)
  }

  def sendGetCompactFiltersMessage(
      filterSyncMarker: FilterSyncMarker): Future[Unit] = {
    val message =
      GetCompactFiltersMessage(if (filterSyncMarker.startHeight < 0) 0
                               else filterSyncMarker.startHeight,
                               filterSyncMarker.stopBlockHash)
    logger.debug(s"Sending getcfilters=$message to peer ${client.peer}")
    sendMsg(message)
  }

  def sendGetCompactFilterHeadersMessage(
      filterSyncMarker: FilterSyncMarker): Future[Unit] = {
    val message =
      GetCompactFilterHeadersMessage(if (filterSyncMarker.startHeight < 0) 0
                                     else filterSyncMarker.startHeight,
                                     filterSyncMarker.stopBlockHash)
    logger.debug(s"Sending getcfheaders=$message to peer ${client.peer}")
    sendMsg(message)
  }

  def sendGetCompactFilterCheckPointMessage(
      stopHash: DoubleSha256Digest): Future[Unit] = {
    val message = GetCompactFilterCheckPointMessage(stopHash)
    logger.debug(s"Sending getcfcheckpt=$message to peer ${client.peer}")
    sendMsg(message)
  }

  private[node] def sendNextGetCompactFilterCommand(
      chainApi: ChainApi,
      filterBatchSize: Int,
      startHeight: Int)(implicit ec: ExecutionContext): Future[Boolean] = {
    for {
      filterSyncMarkerOpt <-
        chainApi.nextFilterHeaderBatchRange(startHeight, filterBatchSize)
      res <- filterSyncMarkerOpt match {
        case Some(filterSyncMarker) =>
          logger.info(s"Requesting compact filters from $filterSyncMarker")

          sendGetCompactFiltersMessage(filterSyncMarker)
            .map(_ => true)
        case None =>
          Future.successful(false)
      }
    } yield res
  }

  private[node] def sendNextGetCompactFilterHeadersCommand(
      chainApi: ChainApi,
      filterHeaderBatchSize: Int,
      prevStopHash: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[Boolean] = {
    for {
      filterSyncMarkerOpt <- chainApi.nextBlockHeaderBatchRange(
        prevStopHash = prevStopHash,
        batchSize = filterHeaderBatchSize)
      res <- filterSyncMarkerOpt match {
        case Some(filterSyncMarker) =>
          logger.info(
            s"Requesting next compact filter headers from $filterSyncMarker")
          sendGetCompactFilterHeadersMessage(filterSyncMarker)
            .map(_ => true)
        case None =>
          Future.successful(false)
      }
    } yield res
  }

  private[node] def sendMsg(msg: NetworkPayload): Future[Unit] = {
    //version or verack messages are the only messages that
    //can be sent before we are fully initialized
    //as they are needed to complete our handshake with our peer
    logger.debug(s"Sending msg=${msg.commandName} to peer=${socket}")
    val newtworkMsg = NetworkMessage(conf.network, msg)
    client.actor ! newtworkMsg
    Future.unit
  }
}

object PeerMessageSender {

  sealed abstract class PeerMessageHandlerMsg

  /** For when we are done with exchanging version and verack messages
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
