package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.p2p.{NetworkPayload, TypeIdentifier}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{
  BroadcastAbleTransaction,
  BroadcastAbleTransactionDAO,
  Peer
}
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.{
  PeerMessageReceiver,
  PeerMessageSender
}
import org.bitcoins.node.util.BitcoinSNodeUtil.Mutable
import org.bitcoins.rpc.util.AsyncUtil
import slick.jdbc.SQLiteProfile

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  This a base trait for various kinds of nodes. It contains house keeping methods required for all nodes.
  */
trait Node extends NodeApi with ChainQueryApi with P2PLogger {

  implicit def system: ActorSystem

  implicit def nodeAppConfig: NodeAppConfig

  implicit def chainAppConfig: ChainAppConfig

  implicit def executionContext: ExecutionContext = system.dispatcher

  val peer: Peer

  private val callbacks = new Mutable(NodeCallbacks.empty)

  def nodeCallbacks: NodeCallbacks = callbacks.atomicGet

  def addCallbacks(newCallbacks: NodeCallbacks): Node = {
    callbacks.atomicUpdate(newCallbacks)(_ + _)
    this
  }

  lazy val txDAO = BroadcastAbleTransactionDAO(SQLiteProfile)

  /** This is constructing a chain api from disk every time we call this method
    * This involves database calls which can be slow and expensive to construct
    * our [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
    * */
  def chainApiFromDb()(
      implicit executionContext: ExecutionContext): Future[ChainHandler] = {
    ChainHandler.fromDatabase(BlockHeaderDAO(),
                              CompactFilterHeaderDAO(),
                              CompactFilterDAO())
  }

  /** Unlike our chain api, this is cached inside our node
    * object. Internally in [[org.bitcoins.node.networking.P2PClient p2p client]] you will see that
    * the [[org.bitcoins.chain.api.ChainApi chain api]] is updated inside of the p2p client
    * */
  lazy val clientF: Future[P2PClient] = {
    for {
      chainApi <- chainApiFromDb()
    } yield {
      val peerMsgRecv: PeerMessageReceiver =
        PeerMessageReceiver.newReceiver(chainApi = chainApi,
                                        peer = peer,
                                        callbacks = nodeCallbacks)
      val p2p = P2PClient(context = system,
                          peer = peer,
                          peerMessageReceiver = peerMsgRecv)
      p2p
    }
  }

  lazy val peerMsgSenderF: Future[PeerMessageSender] = {
    clientF.map { client =>
      PeerMessageSender(client)
    }
  }

  /**
    * Sends the given P2P to our peer.
    * This method is useful for playing around
    * with P2P messages, therefore marked as
    * `private[node]`.
    */
  def send(msg: NetworkPayload): Future[Unit] = {
    peerMsgSenderF.flatMap(_.sendMsg(msg))
  }

  /** Checks if we have a tcp connection with our peer */
  def isConnected: Future[Boolean] = peerMsgSenderF.flatMap(_.isConnected)

  /** Checks if we are fully initialized with our peer and have executed the handshake
    * This means we can now send arbitrary messages to our peer
    *
    * @return
    */
  def isInitialized: Future[Boolean] = peerMsgSenderF.flatMap(_.isInitialized)

  def isDisconnected: Future[Boolean] = peerMsgSenderF.flatMap(_.isDisconnected)

  /** Starts our node */
  def start(): Future[Node] = {
    logger.info("Starting node")
    val start = System.currentTimeMillis()
    for {
      _ <- nodeAppConfig.initialize()
      node <- {
        val isInitializedF = for {
          _ <- peerMsgSenderF.map(_.connect())
          _ <- AsyncUtil.retryUntilSatisfiedF(() => isInitialized)
        } yield ()

        isInitializedF.failed.foreach(err =>
          logger.error(s"Failed to connect with peer=$peer with err=${err}"))

        isInitializedF.map { _ =>
          logger.info(s"Our peer=${peer} has been initialized")
          logger.info(s"Our node has been full started. It took=${System
            .currentTimeMillis() - start}ms")
          this
        }
      }
    } yield {
      node
    }
  }

  /** Stops our node */
  def stop(): Future[Node] = {
    logger.info(s"Stopping node")
    val disconnectF = for {
      p <- peerMsgSenderF
      disconnect <- p.disconnect()
    } yield disconnect

    val start = System.currentTimeMillis()
    val isStoppedF = disconnectF.flatMap { _ =>
      logger.info(s"Awaiting disconnect")
      //25 seconds to disconnect
      AsyncUtil.retryUntilSatisfiedF(() => isDisconnected, 500.millis)
    }

    isStoppedF.failed.foreach { e =>
      logger.warn(s"Cannot stop node", e)
    }

    isStoppedF.map { _ =>
      logger.info(
        s"Node stopped! It took=${System.currentTimeMillis() - start}ms")
      this
    }
  }

  /** Starts to sync our node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    *
    * @return
    */
  def sync(): Future[Unit] = {
    for {
      chainApi <- chainApiFromDb()
      hash <- chainApi.getBestBlockHash()
      header <- chainApi
        .getHeader(hash)
        .map(_.get) // .get is safe since this is an internal call
    } yield {
      peerMsgSenderF.map(_.sendGetHeadersMessage(hash.flip))
      logger.info(s"Starting sync node, height=${header.height} hash=$hash")
    }
  }

  /** Broadcasts the given transaction over the P2P network */
  override def broadcastTransaction(transaction: Transaction): Future[Unit] = {
    val broadcastTx = BroadcastAbleTransaction(transaction)

    txDAO.create(broadcastTx).onComplete {
      case Failure(exception) =>
        logger.error(s"Error when writing broadcastable TX to DB", exception)
      case Success(written) =>
        logger.debug(
          s"Wrote tx=${written.transaction.txIdBE} to broadcastable table")
    }

    logger.info(s"Sending out inv for tx=${transaction.txIdBE}")
    peerMsgSenderF.flatMap(_.sendInventoryMessage(transaction))
  }

  /**
    * Fetches the given blocks from the peers and calls the appropriate [[callbacks]] when done.
    */
  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
    for {
      peerMsgSender <- peerMsgSenderF
      _ <- peerMsgSender.sendGetDataMessage(TypeIdentifier.MsgBlock,
                                            blockHashes: _*)
    } yield ()
  }

  /** Gets the height of the given block */
  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
    chainApiFromDb().flatMap(_.getBlockHeight(blockHash))

  /** Gets the hash of the block that is what we consider "best" */
  override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
    chainApiFromDb().flatMap(_.getBestBlockHash())

  /** Gets number of confirmations for the given block hash*/
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] =
    chainApiFromDb().flatMap(_.getNumberOfConfirmations(blockHashOpt))

}
