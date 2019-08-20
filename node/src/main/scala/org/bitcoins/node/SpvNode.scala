package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.p2p.NetworkPayload
import org.bitcoins.core.protocol.BitcoinAddress
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
import org.bitcoins.rpc.util.AsyncUtil
import slick.jdbc.SQLiteProfile

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

case class SpvNode(
    peer: Peer,
    bloomFilter: BloomFilter,
    callbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty
)(
    implicit system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends P2PLogger {
  import system.dispatcher

  /** This implicit is required for using the [[akka.pattern.ask akka ask]]
    * to query what the state of our node is, like [[isConnected isConnected]]
    * */
  implicit private val timeout = akka.util.Timeout(10.seconds)
  private val txDAO = BroadcastAbleTransactionDAO(SQLiteProfile)

  /** This is constructing a chain api from disk every time we call this method
    * This involves database calls which can be slow and expensive to construct
    * our [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
    * */
  def chainApiFromDb(): Future[ChainApi] = {
    ChainHandler.fromDatabase(BlockHeaderDAO())
  }

  /** Unlike our chain api, this is cached inside our spv node
    * object. Internally in [[org.bitcoins.node.networking.P2PClient p2p client]] you will see that
    * the [[org.bitcoins.chain.api.ChainApi chain api]] is updated inside of the p2p client
    * */
  private val clientF: Future[P2PClient] = {
    for {
      chainApi <- chainApiFromDb()
    } yield {
      val peerMsgRecv: PeerMessageReceiver =
        PeerMessageReceiver.newReceiver(chainApi = chainApi,
                                        peer = peer,
                                        callbacks = callbacks)
      val p2p = P2PClient(context = system,
                          peer = peer,
                          peerMessageReceiver = peerMsgRecv)
      p2p
    }
  }

  private val peerMsgSenderF: Future[PeerMessageSender] = {
    clientF.map { client =>
      PeerMessageSender(client)
    }
  }

  /** Updates our bloom filter to match the given TX
    *
    * @return SPV node with the updated bloom filter
    */
  def updateBloomFilter(transaction: Transaction): Future[SpvNode] = {
    logger.info(s"Updating bloom filter with transaction=${transaction.txIdBE}")
    val newBloom = bloomFilter.update(transaction)

    // we could send filteradd messages, but we would
    // then need to calculate all the new elements in
    // the filter. this is easier:-)
    val newBloomLoadF = peerMsgSenderF.map { p =>
      p.sendFilterClearMessage()
      p.sendFilterLoadMessage(newBloom)
    }

    newBloomLoadF.map(_ => copy(bloomFilter = newBloom))
  }

  /** Updates our bloom filter to match the given address
    *
    * @return SPV node with the updated bloom filter
    */
  def updateBloomFilter(address: BitcoinAddress): Future[SpvNode] = {
    logger.info(s"Updating bloom filter with address=$address")
    val hash = address.hash
    val newBloom = bloomFilter.insert(hash)
    val sentFilterAddF = peerMsgSenderF.map(_.sendFilterAddMessage(hash))

    sentFilterAddF.map { _ =>
      copy(bloomFilter = newBloom)
    }
  }

  /**
    * Sends the given P2P to our peer.
    * This method is useful for playing around
    * with P2P messages, therefore marked as
    * `private[node]`.
    */
  private[node] def send(msg: NetworkPayload): Future[Unit] = {
    peerMsgSenderF.flatMap(_.sendMsg(msg))
  }

  /** Starts our spv node */
  def start(): Future[SpvNode] = {
    logger.info("Starting spv node")
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
          logger.info(s"Our spv node has been full started. It took=${System
            .currentTimeMillis() - start}ms")
          this
        }
      }
      _ <- peerMsgSenderF.map(_.sendFilterLoadMessage(bloomFilter))
    } yield {
      logger.info(s"Sending bloomfilter=${bloomFilter.hex} to $peer")
      node
    }
  }

  /** Stops our spv node */
  def stop(): Future[SpvNode] = {
    logger.info(s"Stopping spv node")
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

    isStoppedF.map { _ =>
      logger.info(
        s"Spv node stopped! It took=${System.currentTimeMillis() - start}ms")
      this
    }
  }

  /** Broadcasts the given transaction over the P2P network */
  def broadcastTransaction(transaction: Transaction): Future[Unit] = {
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

  /** Checks if we have a tcp connection with our peer */
  def isConnected: Future[Boolean] = peerMsgSenderF.flatMap(_.isConnected)

  /** Checks if we are fully initialized with our peer and have executed the handshake
    * This means we can now send arbitrary messages to our peer
    * @return
    */
  def isInitialized: Future[Boolean] = peerMsgSenderF.flatMap(_.isInitialized)

  def isDisconnected: Future[Boolean] = peerMsgSenderF.flatMap(_.isDisconnected)

  /** Starts to sync our spv node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    * @return
    */
  def sync(): Future[Unit] = {
    for {
      chainApi <- chainApiFromDb()
      hash <- chainApi.getBestBlockHash
      header <- chainApi
        .getHeader(hash)
        .map(_.get) // .get is safe since this is an internal call
    } yield {
      peerMsgSenderF.map(_.sendGetHeadersMessage(hash.flip))
      logger.info(s"Starting sync node, height=${header.height} hash=$hash")
    }
  }
}
