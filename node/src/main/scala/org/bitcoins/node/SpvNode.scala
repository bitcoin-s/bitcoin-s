package org.bitcoins.node

import akka.Done
import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.Mutable
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.rpc.util.AsyncUtil

import scala.concurrent.{Future, Promise}

case class SpvNode(
    nodePeer: Peer,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    initialSyncDone: Option[Promise[Done]],
    actorSystem: ActorSystem)
    extends Node {
  require(nodeConfig.nodeType == NodeType.SpvNode,
          s"We need our SPV mode enabled to be able to construct a SPV node!")

  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  override def chainAppConfig: ChainAppConfig = chainConfig

  override val peer: Peer = nodePeer

  private val _bloomFilter = new Mutable(BloomFilter.empty)

  def bloomFilter: BloomFilter = _bloomFilter.atomicGet

  def setBloomFilter(bloom: BloomFilter): SpvNode = {
    _bloomFilter.atomicSet(bloom)
    this
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
    for {
      p <- peerMsgSenderF
      _ <- p.sendFilterClearMessage()
      _ <- p.sendFilterLoadMessage(newBloom)
    } yield this

  }

  /** Updates our bloom filter to match the given address
    *
    * @return SPV node with the updated bloom filter
    */
  def updateBloomFilter(address: BitcoinAddress): Future[SpvNode] = {
    logger.info(s"Updating bloom filter with address=$address")
    val hash = address.hash
    _bloomFilter.atomicUpdate(hash)(_.insert(_))

    val sentFilterAddF = peerMsgSenderF.flatMap(_.sendFilterAddMessage(hash))

    sentFilterAddF.map(_ => this)
  }

  override def start(): Future[SpvNode] = {
    for {
      node <- super.start()
      peerMsgSender <- peerMsgSenderF
      _ <- AsyncUtil.retryUntilSatisfiedF(() => isConnected)
      _ <- peerMsgSender.sendFilterLoadMessage(bloomFilter)
    } yield {
      logger.info(s"Sending bloomfilter=${bloomFilter.hex} to $peer")
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
