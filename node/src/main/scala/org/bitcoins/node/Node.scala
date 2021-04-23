package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandlerCached
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.chain._
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.p2p.TypeIdentifier
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{
  BroadcastAbleTransaction,
  BroadcastAbleTransactionDAO,
  Peer
}
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.{DataMessageHandler, PeerMessageSender}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Random, Success}

/**  This a base trait for various kinds of nodes. It contains house keeping methods required for all nodes.
  */
trait Node extends NodeApi with ChainQueryApi with P2PLogger {

  implicit def system: ActorSystem

  implicit def nodeAppConfig: NodeAppConfig

  implicit def chainAppConfig: ChainAppConfig

  implicit def executionContext: ExecutionContext = system.dispatcher

  def peers: Vector[Peer]

  /** The current data message handler.
    * It should be noted that the dataMessageHandler contains
    * chainstate. When we update with a new chainstate, we need to
    * maek sure we update the [[DataMessageHandler]] via [[updateDataMessageHandler()]]
    * to make sure we don't corrupt our chainstate cache
    */
  def dataMessageHandler: DataMessageHandler

  def nodeCallbacks: NodeCallbacks = nodeAppConfig.nodeCallbacks

  lazy val txDAO: BroadcastAbleTransactionDAO = BroadcastAbleTransactionDAO()

  def updateDataMessageHandler(dataMessageHandler: DataMessageHandler): Node

  def addPeer(peer: Peer): Unit

  def removePeer(peer: Peer): Unit

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

  private[node] lazy val scheduler = nodeAppConfig.scheduler

  /** Unlike our chain api, this is cached inside our node
    * object. Internally in [[org.bitcoins.node.networking.P2PClient p2p client]] you will see that
    * the [[ChainApi chain api]] is updated inside of the p2p client
    */
  def clients: Vector[P2PClient]

  def peerMsgSenders: Vector[PeerMessageSender]

  def randomPeer: PeerMessageSender = {
    peerMsgSenders(Random.nextInt(peerMsgSenders.size))
  }

  /** Checks if we have a tcp connection with all of our peers */
  def isConnected: Future[Boolean] = {
    val connectedFs = peerMsgSenders.map(_.isConnected())
    Future.sequence(connectedFs).map(_.forall(_ == true))
  }

  /** Checks if we are fully initialized with all of our peers and have executed the handshake
    * This means we can now send arbitrary messages to our peers
    *
    * @return
    */
  def isInitialized: Future[Boolean] = {
    val initFs = peerMsgSenders.map(_.isInitialized())
    Future.sequence(initFs).map(_.forall(_ == true))
  }

  def isDisconnected: Future[Boolean] = {
    val disconnectedFs = peerMsgSenders.map(_.isDisconnected())
    Future.sequence(disconnectedFs).map(_.forall(_ == true))
  }

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
      val initFs = peerMsgSenders.map { peerMsgSender =>
        val peer = peerMsgSender.client.peer
        peerMsgSender.connect()
        val isInitializedF = for {
          _ <- AsyncUtil.retryUntilSatisfiedF(
            () => peerMsgSender.isInitialized(),
            interval = 250.millis)
        } yield peer

        isInitializedF.failed.foreach(err =>
          logger.error(s"Failed to connect with peer=$peer with err=$err"))

        isInitializedF.recover { case _: Throwable => removePeer(peer) }

        isInitializedF.map { peer =>
          logger.info(s"Our peer=$peer has been initialized")
        }
      }

      Future.sequence(initFs).map { _ =>
        logger.info(s"Our node has been full started. It took=${System
          .currentTimeMillis() - start}ms")

        scheduler
          .scheduleAtFixedRate(ManagePeersRunnable, 5, 5, TimeUnit.SECONDS)

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
    val disconnectFs = peerMsgSenders.map(_.disconnect())

    val disconnectF = for {
      disconnect <- Future.sequence(disconnectFs)
      _ <- nodeAppConfig.stop()
    } yield disconnect

    val start = System.currentTimeMillis()
    val isStoppedF = disconnectF.flatMap { _ =>
      logger.info(s"Awaiting disconnect")
      //25 seconds to disconnect
      AsyncUtil.retryUntilSatisfiedF(() => isDisconnected, 500.millis, 50)
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

      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE.flip)
      _ <- randomPeer.sendGetHeadersMessage(cachedHeaders)
    } yield {
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE}")
    }
  }

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

    def attemptBroadcast(attemptedPeers: Vector[PeerMessageSender] =
      Vector.empty): Future[Unit] = {
      val peerOpt =
        Random.shuffle(peerMsgSenders).diff(attemptedPeers).headOption

      peerOpt match {
        case Some(peer) =>
          for {
            connected <- peer.isConnected()

            res <- {
              if (connected) {
                logger.info(s"Sending out tx message for tx=$txIds")
                peer.sendInventoryMessage(transactions: _*)
              } else {
                attemptBroadcast(attemptedPeers :+ peer)
              }
            }
          } yield res
        case None =>
          Future.failed(new RuntimeException(
            s"Error broadcasting transaction $txIds, peers are disconnected $peers"))
      }
    }

    for {
      _ <- addToDbF
      res <- attemptBroadcast()
    } yield res
  }

  /** Fetches the given blocks from the peers and calls the appropriate callbacks when done.
    */
  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
    if (blockHashes.isEmpty) {
      Future.unit
    } else {
      randomPeer.sendGetDataMessage(TypeIdentifier.MsgWitnessBlock,
                                    blockHashes: _*)
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

  case object ManagePeersRunnable extends Runnable {

    override def run(): Unit = {

      peerMsgSenders.foreach { peer =>
        peer.isDisconnected().foreach {
          case true  => removePeer(peer.client.peer)
          case false => ()
        }
      }

      val diffMillis =
        System.currentTimeMillis() - dataMessageHandler.lastMessageReceived

      // if it's been 5 seconds since last message
      if (dataMessageHandler.syncing && diffMillis >= 5000) {
        // restart syncing
        logger.warn(
          s"No message from peers for ${diffMillis}ms, restarting syncing")
        sync()
        ()
      }
    }
  }
}
