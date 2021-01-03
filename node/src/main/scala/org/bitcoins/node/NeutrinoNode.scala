package org.bitcoins.node

import akka.Done
import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer

import scala.concurrent.{Future, Promise}

case class NeutrinoNode(
    nodePeer: Peer,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    initialSyncDone: Option[Promise[Done]],
    actorSystem: ActorSystem)
    extends Node {
  require(
    nodeConfig.nodeType == NodeType.NeutrinoNode,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node!")

  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  override def chainAppConfig: ChainAppConfig = chainConfig

  override val peer: Peer = nodePeer

  override def start(): Future[NeutrinoNode] = {
    val res = for {
      node <- super.start()
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash()
      peerMsgSender <- peerMsgSenderF
      _ <- peerMsgSender.sendGetCompactFilterCheckPointMessage(
        stopHash = bestHash.flip)
    } yield {
      node.asInstanceOf[NeutrinoNode]
    }

    res.failed.foreach(logger.error("Cannot start Neutrino node", _))

    res
  }

  /** Starts to sync our node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    *
    * @return
    */
  override def sync(): Future[Unit] = {
    val blockchainsF =
      BlockHeaderDAO()(executionContext, chainConfig).getBlockchains()
    for {
      chainApi <- chainApiFromDb()
      header <- chainApi.getBestBlockHeader()
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      filterCount <- chainApi.getFilterCount()
      peerMsgSender <- peerMsgSenderF
      blockchains <- blockchainsF
    } yield {
      // Get all of our cached headers in case of a reorg
      val cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE.flip)
      peerMsgSender.sendGetHeadersMessage(cachedHeaders)

      // If we have started syncing filters headers
      if (filterHeaderCount != 0) {
        peerMsgSender.sendNextGetCompactFilterHeadersCommand(
          chainApi = chainApi,
          filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
          prevStopHash = header.hashBE)

        // If we have started syncing filters
        if (filterCount != filterHeaderCount)
          peerMsgSender.sendNextGetCompactFilterCommand(
            chainApi = chainApi,
            filterBatchSize = chainConfig.filterBatchSize,
            stopHash = header.hashBE)
      }

      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE.hex}")
    }
  }

  /** Gets the number of compact filters in the database */
  override def getFilterCount(): Future[Int] =
    chainApiFromDb().flatMap(_.getFilterCount())

  /** Returns the block height of the given block stamp */
  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    chainApiFromDb().flatMap(_.getHeightByBlockStamp(blockStamp))

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]] =
    chainApiFromDb().flatMap(_.getFiltersBetweenHeights(startHeight, endHeight))

}
