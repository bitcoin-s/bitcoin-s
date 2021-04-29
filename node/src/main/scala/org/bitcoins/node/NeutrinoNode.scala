package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer._

import scala.collection.mutable
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

  private var peers: Vector[Peer] = _nodePeers

  override def getPeers: Vector[Peer] = peers

  private[this] var dataMessageHandler: DataMessageHandler = _dataMessageHandler

  override def getDataMessageHandler: DataMessageHandler = dataMessageHandler

  override def updateDataMessageHandler(
      dataMessageHandler: DataMessageHandler): NeutrinoNode = {
    this.dataMessageHandler = dataMessageHandler
    this
  }

  private[this] var clients: Vector[P2PClient] = {
    peers.map { peer =>
      val peerMsgRecv: PeerMessageReceiver =
        PeerMessageReceiver.newReceiver(node = this, peer = peer)
      P2PClient(context = system,
                peer = peer,
                peerMessageReceiver = peerMsgRecv)
    }
  }

  private[this] var peerMsgSenders: Vector[PeerMessageSender] = {
    clients.map(PeerMessageSender(_))
  }

  private[this] val peerServices: mutable.Map[Peer, ServiceIdentifier] =
    mutable.Map.empty

  override def getClients: Vector[P2PClient] = clients

  override def getPeerMsgSenders: Vector[PeerMessageSender] = peerMsgSenders

  override def getPeerServices: Map[Peer, ServiceIdentifier] =
    peerServices.toMap

  override def addPeer(peer: Peer): Unit = {
    if (!peers.contains(peer)) {
      val peerMsgRecv: PeerMessageReceiver =
        PeerMessageReceiver.newReceiver(node = this, peer = peer)
      val client = P2PClient(context = system,
                             peer = peer,
                             peerMessageReceiver = peerMsgRecv)
      val peerMsgSender = PeerMessageSender(client)

      peerMsgSender.connect()

      this.peers = peers :+ peer
      this.clients = clients :+ client
      this.peerMsgSenders = peerMsgSenders :+ peerMsgSender
    } else ()
  }

  override def removePeer(peer: Peer): Peer = {
    this.peerMsgSenders = peerMsgSenders.filter(_.client.peer != peer)
    this.clients = clients.filter(_.peer != peer)
    this.peerServices.remove(peer)
    this.peers = peers.filter(_ != peer)
    peer
  }

  override def setPeerServices(
      peer: Peer,
      serviceIdentifier: ServiceIdentifier): Unit = {
    logger.info(s"Setting peer $peer with services $serviceIdentifier")
    peerServices.put(peer, serviceIdentifier)
    ()
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

      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE.flip)
      _ <- randomPeer.sendGetHeadersMessage(cachedHeaders)
    } yield {
      val height = header.height
      if (
        header.height == 0 || height != filterHeaderCount || height != filterCount
      ) {
        updateDataMessageHandler(getDataMessageHandler.copy(syncing = true))
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
