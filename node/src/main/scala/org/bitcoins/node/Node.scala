package org.bitcoins.node

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.chain.blockchain.{ChainHandler}
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
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** This a base trait for various kinds of nodes. It contains house keeping
  * methods required for all nodes.
  */
trait Node
    extends NodeApi
    with ChainQueryApi
    with StartStopAsync[Node]
    with P2PLogger {

  implicit def system: ActorSystem

  implicit def nodeAppConfig: NodeAppConfig

  implicit def chainAppConfig: ChainAppConfig

  implicit def executionContext: ExecutionContext = system.dispatcher

  def peerManager: PeerManager

  def nodeCallbacks: NodeCallbacks = nodeAppConfig.callBacks

  lazy val txDAO: BroadcastAbleTransactionDAO = BroadcastAbleTransactionDAO()

  /** This is constructing a chain api from disk every time we call this method
    * This involves database calls which can be slow and expensive to construct
    * our [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
    */
  def chainApiFromDb()(implicit
      executionContext: ExecutionContext
  ): Future[ChainApi] = {
    val c = ChainHandler.fromDatabase(
      BlockHeaderDAO(),
      CompactFilterHeaderDAO(),
      CompactFilterDAO(),
      ChainStateDescriptorDAO()
    )
    Future.successful(c)
  }

  /** Starts our node */
  override def start(): Future[Node] = {
    logger.info("Starting node")
    val start = System.currentTimeMillis()

    val chainApiF = chainApiFromDb()
    val startNodeF = for {
      _ <- peerManager.start()
    } yield {
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
        s"Started node, best block hash ${bestHash.hex} at height $bestHeight, with $filterHeaderCount filter headers and $filterCount filters. It took=${System
            .currentTimeMillis() - start}ms"
      )
      node
    }
  }

  /** Starts to sync our node with our peer If our local best block hash is the
    * same as our peers we will not sync, otherwise we will keep syncing until
    * our best block hashes match up
    *
    * @return
    *   the peer we are syncing with, or a failed Future if we could not find a
    *   peer to sync with after 5 seconds
    */
  def sync(): Future[Unit]

  /** Broadcasts the given transaction over the P2P network */
  override def broadcastTransactions(
      transactions: Vector[Transaction]
  ): Future[Unit] = {
    val broadcastTxDbs = transactions.map(tx => BroadcastAbleTransaction(tx))

    val addToDbF = txDAO.upsertAll(broadcastTxDbs)

    val txIds = transactions.map(_.txIdBE.hex)

    addToDbF.onComplete {
      case Failure(exception) =>
        logger.error(s"Error when writing broadcastable TXs to DB", exception)
      case Success(written) =>
        logger.debug(
          s"Wrote tx=${written.map(_.transaction.txIdBE.hex)} to broadcastable table"
        )
    }

    for {
      _ <- addToDbF
      _ <- {
        val connected = peerManager.peers.nonEmpty
        if (connected) {
          logger.info(s"Sending out tx message for tx=$txIds")
          val inventories =
            transactions.map(t => Inventory(TypeIdentifier.MsgTx, t.txId))
          val invMsg = InventoryMessage(inventories)
          peerManager.sendToRandomPeer(invMsg)
        } else {
          Future.failed(
            new RuntimeException(
              s"Error broadcasting transaction $txIds, no peers connected"
            )
          )
        }
      }
    } yield ()
  }

  /** Fetches the given blocks from the peers and calls the appropriate
    * [[callbacks]] when done.
    */
  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256DigestBE]
  ): Future[Unit] = {
    if (blockHashes.isEmpty) {
      Future.unit
    } else {
      val typeIdentifier = TypeIdentifier.MsgWitnessBlock
      val inventories =
        blockHashes.map(hash => Inventory(typeIdentifier, hash.flip))
      val message = GetDataMessage(inventories)
      for {
        _ <- peerManager.sendToRandomPeer(message)
      } yield ()
    }
  }

  override def getConnectionCount: Future[Int] = {
    Future.successful(peerManager.connectedPeerCount)
  }

  /** Gets the height of the given block */
  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE
  ): Future[Option[Int]] =
    chainApiFromDb().flatMap(_.getBlockHeight(blockHash))

  /** Gets the hash of the block that is what we consider "best" */
  override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
    chainApiFromDb().flatMap(_.getBestBlockHash())

  /** Gets number of confirmations for the given block hash */
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE
  ): Future[Option[Int]] =
    chainApiFromDb().flatMap(_.getNumberOfConfirmations(blockHashOpt))

  override def epochSecondToBlockHeight(time: Long): Future[Int] =
    chainApiFromDb().flatMap(_.epochSecondToBlockHeight(time))

  override def getMedianTimePast(): Future[Long] =
    chainApiFromDb().flatMap(_.getMedianTimePast())

}
