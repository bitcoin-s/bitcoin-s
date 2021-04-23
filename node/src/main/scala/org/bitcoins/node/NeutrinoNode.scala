package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer._

import scala.concurrent.Future

case class NeutrinoNode(
    private val _nodePeers: Vector[Peer],
    private val _dataMessageHandler: DataMessageHandler,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem)
    extends Node {
  require(
    nodeConfig.nodeType == NodeType.NeutrinoNode,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node!")

  implicit override val system: ActorSystem = actorSystem

  implicit override val nodeAppConfig: NodeAppConfig = nodeConfig

  implicit override val chainAppConfig: ChainAppConfig = chainConfig

  protected var peers: Vector[Peer] = _nodePeers

  protected var dataMessageHandler: DataMessageHandler = _dataMessageHandler

  override def updateDataMessageHandler(
      dataMessageHandler: DataMessageHandler): NeutrinoNode = {
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

  override def start(): Future[NeutrinoNode] = {
    val res = for {
      node <- super.start()
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash()
      _ <- randomPeer.sendGetCompactFilterCheckPointMessage(
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
      blockchains <- blockchainsF
    } yield {
      // Get all of our cached headers in case of a reorg
      val cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE.flip)
      randomPeer.sendGetHeadersMessage(cachedHeaders)

      // If we have started syncing filters headers
      if (filterHeaderCount != 0) {
        randomPeer.sendNextGetCompactFilterHeadersCommand(
          chainApi = chainApi,
          filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
          prevStopHash = header.hashBE)

        // If we have started syncing filters
        if (filterCount != filterHeaderCount && filterCount != 0)
          randomPeer.sendNextGetCompactFilterCommand(
            chainApi = chainApi,
            filterBatchSize = chainConfig.filterBatchSize,
            startHeight = filterCount)
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
