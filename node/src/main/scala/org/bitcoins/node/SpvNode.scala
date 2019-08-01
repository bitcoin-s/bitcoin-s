package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.{
  PeerMessageReceiver,
  PeerMessageSender
}
import org.bitcoins.rpc.util.AsyncUtil

import scala.concurrent.Future
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.p2p.FilterLoadMessage
import org.bitcoins.core.p2p.NetworkPayload
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.node.models.BroadcastAbleTransaction
import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import slick.jdbc.SQLiteProfile
import scala.util.Failure
import scala.util.Success
import org.bitcoins.db.P2PLogger
import org.bitcoins.node.config.NodeAppConfig

case class SpvNode(
    peer: Peer,
    chainApi: ChainApi,
    bloomFilter: BloomFilter,
    callbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty
)(implicit system: ActorSystem, nodeAppConfig: NodeAppConfig)
    extends P2PLogger {
  import system.dispatcher

  private val txDAO = BroadcastAbleTransactionDAO(SQLiteProfile)

  private val peerMsgRecv =
    PeerMessageReceiver.newReceiver(chainApi, callbacks)

  private val client: P2PClient =
    P2PClient(context = system, peer = peer, peerMessageReceiver = peerMsgRecv)

  private val peerMsgSender: PeerMessageSender = {
    PeerMessageSender(client)
  }

  /**
    * Sends the given P2P to our peer.
    * This method is useful for playing around
    * with P2P messages, therefore marked as
    * `private[node]`.
    */
  private[node] def send(msg: NetworkPayload): Unit = {
    peerMsgSender.sendMsg(msg)
  }

  /** Starts our spv node */
  def start(): Future[SpvNode] = {
    for {
      _ <- nodeAppConfig.initialize()
      node <- {
        peerMsgSender.connect()

        val isInitializedF =
          AsyncUtil.retryUntilSatisfied(peerMsgRecv.isInitialized)

        isInitializedF.failed.foreach(err =>
          logger.error(s"Failed to connect with peer=$peer with err=${err}"))

        isInitializedF.map { _ =>
          logger.info(s"Our peer=${peer} has been initialized")
          this
        }
      }
    } yield {
      logger.info(s"Sending bloomfilter=${bloomFilter.hex} to $peer")
      val filterMsg = FilterLoadMessage(bloomFilter)
      val _ = send(filterMsg)
      node
    }
  }

  /** Stops our spv node */
  def stop(): Future[SpvNode] = {
    peerMsgSender.disconnect()

    val isStoppedF = AsyncUtil.retryUntilSatisfied(peerMsgRecv.isDisconnected)

    isStoppedF.map(_ => this)
  }

  /** Broadcasts the given transaction over the P2P network */
  def broadcastTransaction(transaction: Transaction): Unit = {
    val broadcastTx = BroadcastAbleTransaction(transaction)

    txDAO.create(broadcastTx).onComplete {
      case Failure(exception) =>
        logger.error(s"Error when writing broadcastable TX to DB", exception)
      case Success(written) =>
        logger.debug(
          s"Wrote tx=${written.transaction.txIdBE} to broadcastable table")
    }

    logger.info(s"Sending out inv for tx=${transaction.txIdBE}")
    peerMsgSender.sendInventoryMessage(transaction)
  }

  /** Checks if we have a tcp connection with our peer */
  def isConnected: Boolean = peerMsgRecv.isConnected

  /** Checks if we are fully initialized with our peer and have executed the handshake
    * This means we can now send arbitrary messages to our peer
    * @return
    */
  def isInitialized: Boolean = peerMsgRecv.isInitialized

  /** Starts to sync our spv node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    * @return
    */
  def sync(): Future[Unit] = {
    for {
      hash <- chainApi.getBestBlockHash
      header <- chainApi
        .getHeader(hash)
        .map(_.get) // .get is safe since this is an internal call
    } yield {
      peerMsgSender.sendGetHeadersMessage(hash.flip)
      logger.info(s"Starting sync node, height=${header.height} hash=$hash")
    }
  }
}
