package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer

import java.time.Instant
import scala.concurrent.Future

case class NeutrinoNode(
    chainApi: ChainApi,
    walletCreationTimeOpt: Option[Instant],
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem,
    paramPeers: Vector[Peer] = Vector.empty)
    extends Node {
  require(
    nodeConfig.nodeType == NodeType.NeutrinoNode,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node, got=${nodeConfig.nodeType}!")

  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  implicit override def chainAppConfig: ChainAppConfig = chainConfig

  override lazy val peerManager: PeerManager =
    PeerManager(paramPeers, this, walletCreationTimeOpt)

  override def start(): Future[NeutrinoNode] = {
    val res = for {
      node <- super.start()
    } yield {
      node.asInstanceOf[NeutrinoNode]
    }

    res.failed.foreach(logger.error("Cannot start Neutrino node", _))

    res
  }

  override def stop(): Future[NeutrinoNode] =
    super.stop().map(_.asInstanceOf[NeutrinoNode])

  /** Starts to sync our node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    *
    * @return
    */
  override def sync(): Future[Unit] = {
    val serviceIdentifier = ServiceIdentifier.NODE_COMPACT_FILTERS
    //wait for a peer to be available to sync from...
    //due to underlying mutability in PeerManager/PeerFinder
    //we may not have a peer available for selection immediately
    val peerAvailableF = AsyncUtil.retryUntilSatisfiedF(() =>
      peerManager.randomPeerWithService(serviceIdentifier).map(_.isDefined))
    for {
      chainApi <- chainApiFromDb()
      _ <- chainApi.setSyncing(true)
      _ <- peerAvailableF
      _ <- syncHelper(None)
    } yield ()
  }

  /** Helper method to sync the blockchain over the network
    * @param syncPeerOpt if syncPeer is given, we send [[org.bitcoins.core.p2p.GetHeadersMessage]] to that peer. If None we gossip GetHeadersMessage to all peers
    */
  private def syncHelper(syncPeerOpt: Option[Peer]): Future[Unit] = {
    logger.info(s"Syncing with peerOpt=$syncPeerOpt")
    val blockchainsF =
      BlockHeaderDAO()(executionContext, chainConfig).getBlockchains()
    for {
      header <- chainApi.getBestBlockHeader()
      bestFilterHeaderOpt <- chainApi.getBestFilterHeader()
      bestFilterOpt <- chainApi.getBestFilter()
      blockchains <- blockchainsF
      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE)
      _ <- {
        syncPeerOpt match {
          case Some(peer) =>
            peerManager.sendGetHeadersMessage(cachedHeaders, Some(peer))
          case None => peerManager.gossipGetHeadersMessage(cachedHeaders)
        }
      }
      hasStaleTip <- chainApi.isTipStale()
      _ <- {
        if (hasStaleTip) {
          //if we have a stale tip, we will request to sync filter headers / filters
          //after we are done syncing block headers
          Future.unit
        } else {
          syncFilters(bestFilterHeaderOpt = bestFilterHeaderOpt,
                      bestFilterOpt = bestFilterOpt,
                      bestBlockHeader = header,
                      chainApi = chainApi)
        }
      }
    } yield {
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE.hex}")
    }
  }

  private def syncFilters(
      bestFilterHeaderOpt: Option[CompactFilterHeaderDb],
      bestFilterOpt: Option[CompactFilterDb],
      bestBlockHeader: BlockHeaderDb,
      chainApi: ChainApi): Future[Unit] = {
    // If we have started syncing filters headers
    (bestFilterHeaderOpt, bestFilterOpt) match {
      case (None, None) | (None, Some(_)) =>
        //do nothing if we haven't started syncing
        Future.unit
      case (Some(bestFilterHeader), Some(bestFilter)) =>
        val isFilterHeaderSynced =
          bestFilterHeader.blockHashBE == bestBlockHeader.hashBE
        val isFiltersSynced = {
          //check if we have started syncing filters,
          //and if so, see if filter headers and filters
          //were in sync
          bestFilter.hashBE == bestFilterHeader.filterHashBE
        }
        if (isFilterHeaderSynced && isFiltersSynced) {
          //means we are in sync, with filter heads & block headers & filters
          //if there _both_ filter headers and block headers are on
          //an old tip, our event driven node will start syncing
          //filters after block headers are in sync
          //do nothing
          Future.unit
        } else {
          peerManager.syncCompactFilters(bestFilterHeader = bestFilterHeader,
                                         chainApi = chainApi,
                                         bestFilterOpt = Some(bestFilter))
        }
      case (Some(bestFilterHeader), None) =>
        peerManager.syncCompactFilters(bestFilterHeader = bestFilterHeader,
                                       chainApi = chainApi,
                                       bestFilterOpt = None)
    }
  }

  override def syncFromNewPeer(): Future[Option[Peer]] = {
    for {
      syncPeerOpt <- peerManager.randomPeerWithService(
        ServiceIdentifier.NODE_COMPACT_FILTERS)
      _ <- syncHelper(syncPeerOpt)
    } yield syncPeerOpt
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
