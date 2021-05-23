package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.chain.db.CompactFilterHeaderDb
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.DataMessageHandler

import scala.concurrent.Future

case class NeutrinoNode(
    nodePeer: Peer,
    dataMessageHandler: DataMessageHandler,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem)
    extends Node {
  require(
    nodeConfig.nodeType == NodeType.NeutrinoNode,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node!")

  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  override def chainAppConfig: ChainAppConfig = chainConfig

  override val peer: Peer = nodePeer

  override def updateDataMessageHandler(
      dataMessageHandler: DataMessageHandler): NeutrinoNode = {
    copy(dataMessageHandler = dataMessageHandler)
  }

  override def start(): Future[NeutrinoNode] = {
    val res = for {
      node <- super.start()
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash()
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
    for {
      chainApi <- chainApiFromDb()
      header <- chainApi.getBestBlockHeader()
      bestFilterHeaderOpt <- chainApi.getBestFilterHeader()
      filterCount <- chainApi.getFilterCount()
      blockchains <-
        BlockHeaderDAO()(executionContext, chainConfig).getBlockchains()
      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE.flip)
      _ <- peerMsgSender.sendGetHeadersMessage(cachedHeaders)
      _ <- syncFilters(bestFilterHeaderOpt, chainApi, filterCount)
    } yield {
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE.hex}")
    }
  }

  private def syncFilters(
      bestFilterHeaderOpt: Option[CompactFilterHeaderDb],
      chainApi: ChainApi,
      filterCount: Int): Future[Unit] = {
    // If we have started syncing filters headers
    bestFilterHeaderOpt match {
      case None =>
        //do nothing if we haven't started syncing
        Future.unit
      case Some(bestFilterHeader) =>
        val sendCompactFilterHeaderMsgF = {
          peerMsgSender.sendNextGetCompactFilterHeadersCommand(
            chainApi = chainApi,
            filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
            prevStopHash = bestFilterHeader.blockHashBE)
        }
        sendCompactFilterHeaderMsgF.flatMap { isSyncFilterHeaders =>
          // If we have started syncing filters
          if (
            !isSyncFilterHeaders &&
            filterCount != bestFilterHeader.height
            && filterCount != 0
          ) {
            logger.info(s"Starting sync filters in NeutrinoNode.sync()")
            peerMsgSender
              .sendNextGetCompactFilterCommand(chainApi = chainApi,
                                               filterBatchSize =
                                                 chainConfig.filterBatchSize,
                                               startHeight = filterCount)
              .map(_ => ())
          } else {
            Future.unit
          }
        }
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
