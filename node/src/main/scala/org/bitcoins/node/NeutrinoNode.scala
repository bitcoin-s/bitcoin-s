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
  IPv4AddrV2Message,
  NetworkIpAddress,
  ServiceIdentifier
}
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO}
import org.bitcoins.node.networking.peer.DataMessageHandler
import scodec.bits.ByteVector
//import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}
import scala.concurrent.Future

case class NeutrinoNode(
    private var dataMessageHandler: DataMessageHandler,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem
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

  override def handlePeerGossipMessage(message: Any): Unit = {
    message match {
      case addr: AddrMessage =>
        logger.info("got an addr1")
        addr.addresses.foreach(addToPeerQueue)
      case addr: IPv4AddrV2Message =>
        createInDbIfBlockFilterPeer(addr)
        ()
    }
  }

  override def onPeerInitialization(peer: Peer): Future[Unit] = {
    logger.info(s"peer $peer at onPeerInit")
    createInDbIfBlockFilterPeer(peer)
  }

  private def addToPeerQueue(networkAddress: NetworkIpAddress): Unit = {
    if (networkAddress.services.nodeCompactFilters) {
      val bytes = Try(networkAddress.address.ipv4Bytes) match {
        case Failure(_) =>
          networkAddress.address.bytes
        case Success(ipv4Bytes) =>
          ipv4Bytes
      }
      val inetAddress =
        NetworkUtil.parseInetSocketAddress(bytes, networkAddress.port)
      val peer = Peer.fromSocket(socket = inetAddress,
                                 socks5ProxyParams =
                                   nodeAppConfig.socks5ProxyParams)
      logger.info(s"adding to q: $peer")
      peersToCheckQueue.enqueue(peer)
    }
  }

  private def createInDbIfBlockFilterPeer(
      addr: IPv4AddrV2Message): Future[Unit] = {
    val serviceIdentifier = ServiceIdentifier.fromBytes(addr.services.bytes)
    if (serviceIdentifier.nodeCompactFilters) {
      logger.debug(s"Peer from addr: ${addr.addr} supports compact filters.")
      PeerDAO()
        .upsertPeer(addr.addrBytes, addr.port.toInt, addr.networkId)
        .map(_ => ())
    } else Future.unit
  }

  private def createInDbIfBlockFilterPeer(peer: Peer): Future[Unit] = {
    if (peerData(peer).serviceIdentifier.nodeCompactFilters) {
      logger.info(s"Our peer=$peer has been initialized and upport cf")
      val addrBytes = peer.socket.getAddress.getAddress
      val networkByte =
        if (addrBytes.length == AddrV2Message.IPV4_ADDR_LENGTH) {
          AddrV2Message.IPV4_NETWORK_BYTE
        } else if (addrBytes.length == AddrV2Message.IPV6_ADDR_LENGTH) {
          AddrV2Message.IPV6_NETWORK_BYTE
        } else // todo might be tor, how to handle?
          throw new IllegalArgumentException("Unknown peer network")
      logger.info(s"Adding peer to db $peer")
      PeerDAO()
        .upsertPeer(ByteVector(addrBytes), peer.socket.getPort, networkByte)
        .map(_ => ())
      Future.unit
    } else {
      logger.info(
        s"Our peer=$peer does not support compact filters. Disconnecting.")
      removePeer(peer)
      PeerDAO()
        .deleteByKey(s"${peer.socket.getHostString}:${peer.socket.getPort}")
        .map(_ => ())
    }
  }

  override def start(): Future[NeutrinoNode] = {
    val res = for {
      node <- super.start()
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash()
      _ <- randomPeerMsgSenderWithCompactFilters
        .sendGetCompactFilterCheckPointMessage(stopHash = bestHash.flip)
    } yield {
      logger.debug(s"All peers $peers")
      logger.debug(
        s"Peers with compact filters ${peers.filter(peerData(_).serviceIdentifier.nodeCompactFilters)}")
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
