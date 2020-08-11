package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.p2p.{NetworkPayload, TypeIdentifier}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
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
import org.bitcoins.rpc.util.AsyncUtil

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**
  *  This a base trait for various kinds of nodes. It contains house keeping methods required for all nodes.
  */
trait Node extends NodeApi with ChainQueryApi with P2PLogger {

  implicit def system: ActorSystem

  implicit def nodeAppConfig: NodeAppConfig

  implicit def chainAppConfig: ChainAppConfig

  implicit def executionContext: ExecutionContext = system.dispatcher

  val peer: Peer

  def nodeCallbacks: NodeCallbacks = nodeAppConfig.nodeCallbacks

  lazy val txDAO = BroadcastAbleTransactionDAO()

  /** This is constructing a chain api from disk every time we call this method
    * This involves database calls which can be slow and expensive to construct
    * our [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
    */
  def chainApiFromDb()(implicit
      executionContext: ExecutionContext): Future[ChainHandler] = {
    ChainHandler.fromDatabase(BlockHeaderDAO(),
                              CompactFilterHeaderDAO(),
                              CompactFilterDAO())
  }

  /** Unlike our chain api, this is cached inside our node
    * object. Internally in [[org.bitcoins.node.networking.P2PClient p2p client]] you will see that
    * the [[org.bitcoins.chain.api.ChainApi chain api]] is updated inside of the p2p client
    */
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
      _ <- nodeAppConfig.start()
      // get chainApi so we don't need to call chainApiFromDb on every call
      chainApi <- chainApiFromDb
      node <- {
        val isInitializedF = for {
          _ <- peerMsgSenderF.map(_.connect())
          _ <- AsyncUtil.retryUntilSatisfiedF(() => isInitialized)
        } yield ()

        isInitializedF.failed.foreach(err =>
          logger.error(s"Failed to connect with peer=$peer with err=${err}"))

        isInitializedF.map { _ =>
          logger.info(s"Our peer=$peer has been initialized")
          logger.info(s"Our node has been full started. It took=${System
            .currentTimeMillis() - start}ms")
          this
        }
      }

      _ = logger.trace("Fetching node starting point")
      bestHash <- chainApi.getBestBlockHash()
      bestHeight <- chainApi.getBestHashBlockHeight()
      filterCount <- chainApi.getFilterCount
      filterHeaderCount <- chainApi.getFilterHeaderCount
    } yield {
      logger.info(
        s"Started node, best block hash ${bestHash.hex} at height $bestHeight, with $filterHeaderCount filter headers and $filterCount filters")
      node
    }
  }

  /** Stops our node */
  def stop(): Future[Node] = {
    logger.info(s"Stopping node")
    val disconnectF = for {
      _ <- nodeAppConfig.stop()
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
      header <- chainApi.getBestBlockHeader()
    } yield {
      // Get all of our cached headers in case of a reorg
      val cachedHeaders =
        chainApi.blockchains.flatMap(_.headers).map(_.hashBE.flip)
      peerMsgSenderF.map(_.sendGetHeadersMessage(cachedHeaders))
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE}")
    }
  }

  /** Broadcasts the given transaction over the P2P network */
  override def broadcastTransaction(transaction: Transaction): Future[Unit] = {
    val broadcastTx = BroadcastAbleTransaction(transaction)

    txDAO.upsert(broadcastTx).onComplete {
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
    if (blockHashes.isEmpty) {
      FutureUtil.unit
    } else {
      for {
        peerMsgSender <- peerMsgSenderF
        _ <- peerMsgSender.sendGetDataMessage(TypeIdentifier.MsgBlock,
                                              blockHashes: _*)
      } yield ()
    }
  }

  /** Gets the height of the given block */
  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
    chainApiFromDb().flatMap(_.getBlockHeight(blockHash))

  /** Gets the hash of the block that is what we consider "best" */
  override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
    chainApiFromDb().flatMap(_.getBestBlockHash())

  /** Gets number of confirmations for the given block hash */
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] =
    chainApiFromDb().flatMap(_.getNumberOfConfirmations(blockHashOpt))

  override def epochSecondToBlockHeight(time: Long): Future[Int] =
    chainApiFromDb().flatMap(_.epochSecondToBlockHeight(time))

}
