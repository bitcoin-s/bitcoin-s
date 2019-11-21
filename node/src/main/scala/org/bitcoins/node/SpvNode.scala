package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer

import scala.concurrent.Future

case class SpvNode(
    nodePeer: Peer,
    bloomFilter: BloomFilter,
    nodeCallbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem)
    extends Node {
  require(nodeConfig.isSPVEnabled,
          s"We need our SPV mode enabled to be able to construct a SPV node!")

  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  implicit override def chainAppConfig: ChainAppConfig = chainConfig

  override val peer: Peer = nodePeer

  override val callbacks: SpvNodeCallbacks = nodeCallbacks

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

  override def start(): Future[Node] = {
    for {
      node <- super.start()
      peerMsgSender <- peerMsgSenderF
      _ <- peerMsgSender.sendFilterLoadMessage(bloomFilter)
    } yield {
      logger(nodeAppConfig).info(
        s"Sending bloomfilter=${bloomFilter.hex} to $peer")
      logger.info(s"Sending bloomfilter=${bloomFilter.hex} to $peer")
      node
    }
  }

  override def rescan(
      scriptPubKeysToWatch: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp]): Future[Unit] =
    Future.failed(new RuntimeException("Rescan not implemented"))
}
