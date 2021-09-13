package org.bitcoins.node

import akka.actor.ActorSystem
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
import org.bitcoins.core.p2p.{
  AddrMessage,
  AddrV2Message,
  GossipAddrMessage,
  IPv4AddrV2Message,
  IPv6AddrV2Message,
  NetworkIpAddress,
  ServiceIdentifier,
  TorV3AddrV2Message
}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDB}
import org.bitcoins.node.networking.peer.DataMessageHandler
import scodec.bits.ByteVector

import java.net.InetAddress
import scala.util.{Failure, Success, Try}
import scala.concurrent.Future

case class NeutrinoNode(
    private var dataMessageHandler: DataMessageHandler,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem,
    confPeersOverride: Vector[Peer] = Vector.empty
) extends Node {
  require(
    nodeConfig.nodeType == NodeType.NeutrinoNode,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node!")

  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  override def chainAppConfig: ChainAppConfig = chainConfig

  override def getDataMessageHandler: DataMessageHandler = dataMessageHandler

  override def updateDataMessageHandler(
      dataMessageHandler: DataMessageHandler): NeutrinoNode = {
    this.dataMessageHandler = dataMessageHandler
    this
  }

  override def getPeersFromConf: Vector[Peer] = {
    if (confPeersOverride.isEmpty) super.getPeersFromConf
    else confPeersOverride
  }

  override def handlePeerGossipMessage(message: GossipAddrMessage): Unit = {
    message match {
      case addr: AddrMessage =>
        addr.addresses.foreach(addToPeerQueue)
      case addr: AddrV2Message =>
        val bytes = addr.bytes
        val port = addr.port.toInt
        val services = ServiceIdentifier.fromBytes(addr.services.bytes)
        val inetAddress =
          NetworkUtil.parseInetSocketAddress(bytes, port)
        val peer = Peer.fromSocket(socket = inetAddress,
                                   socks5ProxyParams =
                                     nodeAppConfig.socks5ProxyParams)
        addr match {
          case IPv4AddrV2Message(_, _, _, _) | IPv6AddrV2Message(_, _, _, _) =>
            if (services.nodeCompactFilters)
              peersToCheckStack.push(peer)
          case TorV3AddrV2Message(_, _, _, _) =>
            if (nodeConfig.torConf.enabled && services.nodeCompactFilters)
              peersToCheckStack.push(peer)
          case _ => logger.debug("Unsupported network. Skipping.")
        }
    }
  }

  override def onPeerInitialization(peer: Peer): Future[Unit] = {
    //if its not removed then it means it means initialization was successful as in failed initializations are removed
    //from peerData
    if (
      peerData.contains(peer) && peerData(
        peer).serviceIdentifier.nodeCompactFilters
    ) {
      for {
        _ <- createInDb(peer)
      } yield {
        if (
          peerData(
            peer).keepConnection || connectedPeersCount < maxConnectedPeers
        ) {
          peerData(peer).peerMessageSender.sendGetAddrMessage()
          //only the nodes that have keepConnection as true would be actually used by us
          peerData(peer).keepConnection = true
          logger.info(
            s"Connected to peer $peer with compact filters. Connected peer count $connectedPeersCount")
        } else removePeer(peer)
      }
    } else {
      removePeer(peer)
      Future.unit
    }
  }

  private def addToPeerQueue(networkAddress: NetworkIpAddress): Unit = {
    if (networkAddress.services.nodeCompactFilters) {
      val bytes = Try(networkAddress.address.ipv4Bytes) match {
        case Failure(_) =>
          networkAddress.address.bytes
        case Success(ipv4Bytes) =>
          ipv4Bytes
      }
      logger.debug(s"Peer from addr: $bytes")
      val inetAddress =
        NetworkUtil.parseInetSocketAddress(bytes, networkAddress.port)
      val peer = Peer.fromSocket(socket = inetAddress,
                                 socks5ProxyParams =
                                   nodeAppConfig.socks5ProxyParams)
      peersToCheckStack.push(peer)
    }
  }

  private def createInDb(peer: Peer): Future[PeerDB] = {
    logger.debug(s"Adding peer to db $peer")
    val addrBytes =
      if (peer.socket.getHostString.contains(".onion"))
        NetworkUtil.torV3AddressToBytes(peer.socket.getHostString)
      else
        InetAddress.getByName(peer.socket.getHostString).getAddress
    val networkByte = addrBytes.length match {
      case AddrV2Message.IPV4_ADDR_LENGTH   => AddrV2Message.IPV4_NETWORK_BYTE
      case AddrV2Message.IPV6_ADDR_LENGTH   => AddrV2Message.IPV6_NETWORK_BYTE
      case AddrV2Message.TOR_V3_ADDR_LENGTH => AddrV2Message.TOR_V3_NETWORK_BYTE
      case _                                => throw new IllegalArgumentException("Unsupported address type")
    }
    PeerDAO()
      .upsertPeer(ByteVector(addrBytes), peer.socket.getPort, networkByte)
  }

  override def start(): Future[NeutrinoNode] = {
    val res = for {
      node <- super.start()
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash()
      _ <- randomPeerMsgSenderWithCompactFilters
        .sendGetCompactFilterCheckPointMessage(stopHash = bestHash.flip)
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
      bestFilterHeaderOpt <- chainApi.getBestFilterHeader()
      bestFilterOpt <- chainApi.getBestFilter()
      blockchains <- blockchainsF
      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE.flip)
      _ <- randomPeerMsgSender.sendGetHeadersMessage(cachedHeaders)
      _ <- syncFilters(bestFilterHeaderOpt = bestFilterHeaderOpt,
                       bestFilterOpt = bestFilterOpt,
                       bestBlockHeader = header,
                       chainApi = chainApi)
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
          syncCompactFilters(bestFilterHeader, chainApi, Some(bestFilter))
        }
      case (Some(bestFilterHeader), None) =>
        syncCompactFilters(bestFilterHeader, chainApi, None)
    }
  }

  /** Starts sync compact filer headers.
    * Only starts syncing compact filters if our compact filter headers are in sync with block headers
    */
  private def syncCompactFilters(
      bestFilterHeader: CompactFilterHeaderDb,
      chainApi: ChainApi,
      bestFilterOpt: Option[CompactFilterDb]): Future[Unit] = {
    val sendCompactFilterHeaderMsgF = {
      randomPeerMsgSenderWithCompactFilters
        .sendNextGetCompactFilterHeadersCommand(
          chainApi = chainApi,
          filterHeaderBatchSize = chainConfig.filterHeaderBatchSize,
          prevStopHash = bestFilterHeader.blockHashBE)
    }
    sendCompactFilterHeaderMsgF.flatMap { isSyncFilterHeaders =>
      // If we have started syncing filters
      if (
        !isSyncFilterHeaders &&
        bestFilterOpt.isDefined &&
        bestFilterOpt.get.hashBE != bestFilterHeader.filterHashBE
      ) {
        //means we are not syncing filter headers, and our filters are NOT
        //in sync with our compact filter headers
        logger.info(s"Starting sync filters in NeutrinoNode.sync()")
        randomPeerMsgSenderWithCompactFilters
          .sendNextGetCompactFilterCommand(
            chainApi = chainApi,
            filterBatchSize = chainConfig.filterBatchSize,
            startHeight = bestFilterOpt.get.height)
          .map(_ => ())
      } else {
        Future.unit
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
