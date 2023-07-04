package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer

import java.time.Instant
import scala.concurrent.Future

case class NeutrinoNode(
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
    PeerManager(paramPeers, walletCreationTimeOpt)

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
    val peerAvailableF = AsyncUtil.retryUntilSatisfied(
      peerManager.randomPeerWithService(serviceIdentifier).isDefined)
    for {
      chainApi <- chainApiFromDb()
      _ <- chainApi.setSyncing(true)
      _ <- peerAvailableF
      _ <- peerManager.syncHelper(None)
    } yield ()
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
