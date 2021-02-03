package org.bitcoins.node

import akka.Done
import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.ChainHandlerCached
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.chain._
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
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

/**  This a base trait for various kinds of nodes. It contains house keeping methods required for all nodes.
  */
trait Node extends NodeApi with ChainQueryApi with P2PLogger {

  implicit def system: ActorSystem

  implicit def nodeAppConfig: NodeAppConfig

  implicit def chainAppConfig: ChainAppConfig

  implicit def executionContext: ExecutionContext = system.dispatcher

  val peer: Peer

  protected val initialSyncDone: Option[Promise[Done]]

  def nodeCallbacks: NodeCallbacks = nodeAppConfig.nodeCallbacks

  lazy val txDAO: BroadcastAbleTransactionDAO = BroadcastAbleTransactionDAO()

  /** This is constructing a chain api from disk every time we call this method
    * This involves database calls which can be slow and expensive to construct
    * our [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
    */
  def chainApiFromDb()(implicit
      executionContext: ExecutionContext): Future[ChainHandlerCached] = {
    ChainHandlerCached.fromDatabase(BlockHeaderDAO(),
                                    CompactFilterHeaderDAO(),
                                    CompactFilterDAO())
  }

  /** Unlike our chain api, this is cached inside our node
    * object. Internally in [[org.bitcoins.node.networking.P2PClient p2p client]] you will see that
    * the [[ChainApi chain api]] is updated inside of the p2p client
    */
  lazy val clientF: Future[P2PClient] = {
    val chainApiF = chainApiFromDb()
    for {
      chainApi <- chainApiF
    } yield {
      val peerMsgRecv: PeerMessageReceiver =
        PeerMessageReceiver.newReceiver(chainApi = chainApi,
                                        peer = peer,
                                        initialSyncDone = initialSyncDone)
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

  /** Sends the given P2P to our peer.
    * This method is useful for playing around
    * with P2P messages, therefore marked as
    * `private[node]`.
    */
  def send(msg: NetworkPayload): Future[Unit] = {
    peerMsgSenderF.flatMap(_.sendMsg(msg))
  }

  /** Checks if we have a tcp connection with our peer */
  def isConnected: Future[Boolean] = peerMsgSenderF.flatMap(_.isConnected())

  /** Checks if we are fully initialized with our peer and have executed the handshake
    * This means we can now send arbitrary messages to our peer
    *
    * @return
    */
  def isInitialized: Future[Boolean] = peerMsgSenderF.flatMap(_.isInitialized())

  def isDisconnected: Future[Boolean] =
    peerMsgSenderF.flatMap(_.isDisconnected())

  /** Starts our node */
  def start(): Future[Node] = {
    logger.info("Starting node")
    val start = System.currentTimeMillis()

    val startConfsF = for {
      _ <- chainAppConfig.start()
      _ <- nodeAppConfig.start()
    } yield ()

    val chainApiF = startConfsF.flatMap(_ => chainApiFromDb())

    val startNodeF = {
      val isInitializedF = for {
        _ <- peerMsgSenderF.map(_.connect())
        _ <- AsyncUtil.retryUntilSatisfiedF(() => isInitialized,
                                            interval = 250.millis)
      } yield ()

      isInitializedF.failed.foreach(err =>
        logger.error(s"Failed to connect with peer=$peer with err=$err"))

      isInitializedF.map { _ =>
        logger.info(s"Our peer=$peer has been initialized")
        logger.info(s"Our node has been full started. It took=${System
          .currentTimeMillis() - start}ms")
        this
      }
    }

    val bestHashF = chainApiF.flatMap(_.getBestBlockHash())
    val bestHeightF = chainApiF.flatMap(_.getBestHashBlockHeight())
    val filterHeaderCountF = chainApiF.flatMap(_.getFilterHeaderCount())
    val filterCountF = chainApiF.flatMap(_.getFilterCount())

    for {
      _ <- startConfsF
      node <- startNodeF

      _ = logger.trace("Fetching node starting point")
      bestHash <- bestHashF
      bestHeight <- bestHeightF
      filterHeaderCount <- filterHeaderCountF
      filterCount <- filterCountF
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
      p <- peerMsgSenderF
      disconnect <- p.disconnect()
      _ <- nodeAppConfig.stop()
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
    val blockchainsF =
      BlockHeaderDAO()(executionContext, chainAppConfig).getBlockchains()
    for {
      chainApi <- chainApiFromDb()
      header <- chainApi.getBestBlockHeader()
      blockchains <- blockchainsF
    } yield {
      // Get all of our cached headers in case of a reorg
      val cachedHeaders =
        blockchains.flatMap(_.headers).map(_.hashBE.flip)
      peerMsgSenderF.map(_.sendGetHeadersMessage(cachedHeaders))
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE}")
    }
  }

  /** Broadcasts the given transaction over the P2P network */
  override def broadcastTransaction(transaction: Transaction): Future[Unit] = {
    val broadcastTx = BroadcastAbleTransaction(transaction)

    val addToDbF = txDAO.upsert(broadcastTx)

    addToDbF.onComplete {
      case Failure(exception) =>
        logger.error(s"Error when writing broadcastable TX to DB", exception)
      case Success(written) =>
        logger.debug(
          s"Wrote tx=${written.transaction.txIdBE.hex} to broadcastable table")
    }

    for {
      _ <- addToDbF
      peerMsgSender <- peerMsgSenderF

      // Note: This is a privacy leak and should be fixed in the future. Ideally, we should
      // be using an inventory message to broadcast the transaction to help hide the fact that
      // this transaction belongs to us. However, currently it is okay for us to use a transaction
      // message because a Bitcoin-S node currently doesn't have a mempool and only
      // broadcasts/relays transactions from its own wallet.
      // See https://developer.bitcoin.org/reference/p2p_networking.html#tx
      connected <- isConnected

      res <- {
        if (connected) {
          logger.info(
            s"Sending out tx message for tx=${transaction.txIdBE.hex}")
          peerMsgSender.sendTransactionMessage(transaction)
        } else {
          Future.failed(new RuntimeException(
            s"Error broadcasting transaction ${transaction.txIdBE.hex}, peer is disconnected $peer"))
        }
      }
    } yield res
  }

  /** Fetches the given blocks from the peers and calls the appropriate [[callbacks]] when done.
    */
  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
    if (blockHashes.isEmpty) {
      FutureUtil.unit
    } else {
      for {
        peerMsgSender <- peerMsgSenderF
        _ <- peerMsgSender.sendGetDataMessage(TypeIdentifier.MsgWitnessBlock,
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
