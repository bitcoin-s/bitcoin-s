package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.blockchain.ChainHandlerCached
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  ChainStateDescriptorDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.chain._
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models._
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.{
  ControlMessageHandler,
  DataMessageHandler,
  PeerMessageSender
}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/**  This a base trait for various kinds of nodes. It contains house keeping methods required for all nodes.
  */
trait Node extends NodeApi with ChainQueryApi with P2PLogger {

  implicit def system: ActorSystem

  implicit def nodeAppConfig: NodeAppConfig

  implicit def chainAppConfig: ChainAppConfig

  implicit def executionContext: ExecutionContext = system.dispatcher

  val peerManager: PeerManager

  /** The current data message handler.
    * It should be noted that the dataMessageHandler contains
    * chainstate. When we update with a new chainstate, we need to
    * make sure we update the [[DataMessageHandler]] via [[updateDataMessageHandler()]]
    * to make sure we don't corrupt our chainstate cache
    */
  def getDataMessageHandler: DataMessageHandler

  def controlMessageHandler: ControlMessageHandler

  def nodeCallbacks: NodeCallbacks = nodeAppConfig.callBacks

  lazy val txDAO: BroadcastAbleTransactionDAO = BroadcastAbleTransactionDAO()

  def updateDataMessageHandler(dataMessageHandler: DataMessageHandler): Node

  /** This is constructing a chain api from disk every time we call this method
    * This involves database calls which can be slow and expensive to construct
    * our [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
    */
  def chainApiFromDb()(implicit
      executionContext: ExecutionContext): Future[ChainHandlerCached] = {
    ChainHandlerCached.fromDatabase(BlockHeaderDAO(),
                                    CompactFilterHeaderDAO(),
                                    CompactFilterDAO(),
                                    ChainStateDescriptorDAO())
  }

  /** Unlike our chain api, this is cached inside our node
    * object. Internally in [[org.bitcoins.node.networking.P2PClient p2p client]] you will see that
    * the [[ChainApi chain api]] is updated inside of the p2p client
    */
  def clients: Vector[P2PClient] = peerManager.clients

  def peerMsgSenders: Vector[PeerMessageSender] = peerManager.peerMsgSenders

  /** Sends the given P2P to our peer.
    * This method is useful for playing around
    * with P2P messages, therefore marked as
    * `private[node]`.
    */
  def send(msg: NetworkPayload, peer: Peer): Future[Unit] = {
    peerManager.peerData(peer).peerMessageSender.sendMsg(msg)
  }

  /** Starts our node */
  def start(): Future[Node] = {
    logger.info("Starting node")
    val start = System.currentTimeMillis()

    val chainApiF = chainApiFromDb()
    val startNodeF = for {
      _ <- peerManager.start()
    } yield {
      logger.info(s"Our node has been full started. It took=${System
        .currentTimeMillis() - start}ms")
      this
    }

    val bestHashF = chainApiF.flatMap(_.getBestBlockHash())
    val bestHeightF = chainApiF.flatMap(_.getBestHashBlockHeight())
    val filterHeaderCountF = chainApiF.flatMap(_.getFilterHeaderCount())
    val filterCountF = chainApiF.flatMap(_.getFilterCount())

    for {
      node <- startNodeF
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

    val start = System.currentTimeMillis()

    peerManager.stop().map { _ =>
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
  def sync(): Future[Unit]

  /** Broadcasts the given transaction over the P2P network */
  override def broadcastTransactions(
      transactions: Vector[Transaction]): Future[Unit] = {
    val broadcastTxDbs = transactions.map(tx => BroadcastAbleTransaction(tx))

    val addToDbF = txDAO.upsertAll(broadcastTxDbs)

    val txIds = transactions.map(_.txIdBE.hex)

    addToDbF.onComplete {
      case Failure(exception) =>
        logger.error(s"Error when writing broadcastable TXs to DB", exception)
      case Success(written) =>
        logger.debug(
          s"Wrote tx=${written.map(_.transaction.txIdBE.hex)} to broadcastable table")
    }

    for {
      _ <- addToDbF
      _ <- {
        val connected = peerManager.peers.nonEmpty
        if (connected) {
          logger.info(s"Sending out tx message for tx=$txIds")
          Future.sequence(
            peerMsgSenders.map(_.sendInventoryMessage(transactions: _*)))
        } else {
          Future.failed(
            new RuntimeException(
              s"Error broadcasting transaction $txIds, no peers connected"))
        }
      }
    } yield ()
  }

  /** Fetches the given blocks from the peers and calls the appropriate [[callbacks]] when done.
    */
  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
    if (blockHashes.isEmpty) {
      Future.unit
    } else {
      val syncPeerOpt = getDataMessageHandler.syncPeer
      syncPeerOpt match {
        case Some(peer) =>
          peerManager
            .peerData(peer)
            .peerMessageSender
            .sendGetDataMessage(TypeIdentifier.MsgWitnessBlock, blockHashes: _*)
        case None =>
          throw new RuntimeException(
            "IBD not started yet. Cannot query for blocks.")
      }
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

  override def getMedianTimePast(): Future[Long] =
    chainApiFromDb().flatMap(_.getMedianTimePast())

}
