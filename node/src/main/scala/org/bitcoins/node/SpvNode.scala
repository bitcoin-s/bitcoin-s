package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.Mutable
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer._

import scala.concurrent.Future

case class SpvNode(
    private val _nodePeers: Vector[Peer],
    private val _dataMessageHandler: DataMessageHandler,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem)
    extends Node {
  require(nodeConfig.nodeType == NodeType.SpvNode,
          s"We need our SPV mode enabled to be able to construct a SPV node!")

  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  override def chainAppConfig: ChainAppConfig = chainConfig

  protected var peers: Vector[Peer] = _nodePeers

  private val _bloomFilter = new Mutable(BloomFilter.empty)

  def bloomFilter: BloomFilter = _bloomFilter.atomicGet

  def setBloomFilter(bloom: BloomFilter): SpvNode = {
    _bloomFilter.atomicSet(bloom)
    this
  }

  var dataMessageHandler: DataMessageHandler = _dataMessageHandler

  override def updateDataMessageHandler(
      dataMessageHandler: DataMessageHandler): SpvNode = {
    this.dataMessageHandler = dataMessageHandler
    this
  }

  var clients: Vector[P2PClient] = {
    peers.map { peer =>
      val peerMsgRecv: PeerMessageReceiver =
        PeerMessageReceiver.newReceiver(node = this, peer = peer)
      P2PClient(context = system,
                peer = peer,
                peerMessageReceiver = peerMsgRecv)
    }
  }

  var peerMsgSenders: Vector[PeerMessageSender] = {
    clients.map { client =>
      PeerMessageSender(client)
    }
  }

  def addPeer(peer: Peer): Unit = {
    if (!peers.contains(peer)) {
      val peerMsgRecv: PeerMessageReceiver =
        PeerMessageReceiver.newReceiver(node = this, peer = peer)
      val client = P2PClient(context = system,
                             peer = peer,
                             peerMessageReceiver = peerMsgRecv)
      val peerMsgSender = PeerMessageSender(client)

      this.peers = peers :+ peer
      this.clients = clients :+ client
      this.peerMsgSenders = peerMsgSenders :+ peerMsgSender
    } else ()
  }

  def removePeer(peer: Peer): Peer = {
    this.peers = peers.filter(_ != peer)
    this.clients = clients.filter(_.peer != peer)
    this.peerMsgSenders = peerMsgSenders.filter(_.client.peer != peer)
    peer
  }

  /** Updates our bloom filter to match the given TX
    *
    * @return SPV node with the updated bloom filter
    */
  def updateBloomFilter(transaction: Transaction): Future[SpvNode] = {
    logger.info(s"Updating bloom filter with transaction=${transaction.txIdBE}")
    val newBloom = _bloomFilter.atomicUpdate(transaction)(_.update(_))

    // we could send filteradd messages, but we would
    // then need to calculate all the new elements in
    // the filter. this is easier:-)
    val updateFs = peerMsgSenders.map { peerMsgSender =>
      for {
        _ <- peerMsgSender.sendFilterClearMessage()
        _ <- peerMsgSender.sendFilterLoadMessage(newBloom)
      } yield ()
    }

    Future.sequence(updateFs).map(_ => this)
  }

  /** Updates our bloom filter to match the given address
    *
    * @return SPV node with the updated bloom filter
    */
  def updateBloomFilter(address: BitcoinAddress): Future[SpvNode] = {
    logger.info(s"Updating bloom filter with address=$address")
    val hash = address.hash
    _bloomFilter.atomicUpdate(hash)(_.insert(_))

    val sentFilterAddFs = peerMsgSenders.map(_.sendFilterAddMessage(hash))

    Future.sequence(sentFilterAddFs).map(_ => this)
  }

  override def start(): Future[SpvNode] = {
    for {
      node <- super.start()
      _ <- AsyncUtil.retryUntilSatisfiedF(() => isConnected)
      loadMsgFs = peerMsgSenders.map(_.sendFilterLoadMessage(bloomFilter))
      _ <- Future.sequence(loadMsgFs)
    } yield {
      logger.info(s"Sending bloomfilter=${bloomFilter.hex} to $peers")
      node.asInstanceOf[SpvNode]
    }
  }

  /** Returns the block height of the given block stamp */
  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    chainApiFromDb().flatMap(_.getHeightByBlockStamp(blockStamp))

  private val cfErrMsg = "Compact filters are not supported in SPV mode"

  /** Gets the number of compact filters in the database */
  override def getFilterCount(): Future[Int] =
    Future.failed(new RuntimeException(cfErrMsg))

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]] =
    Future.failed(new RuntimeException(cfErrMsg))
}
