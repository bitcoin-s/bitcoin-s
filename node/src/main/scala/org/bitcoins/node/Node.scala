package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandlerCached
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.chain._
import org.bitcoins.core.api.node.{NodeApi, NodeType}
import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models._
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.{
  DataMessageHandler,
  PeerMessageReceiver,
  PeerMessageSender
}
import scodec.bits.ByteVector

import java.net.{InetAddress, UnknownHostException}
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success, Try}

case class PeerData(peer: Peer, node: Node, reconnect: Boolean)(implicit
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig) {

  lazy val peerMessageSender: PeerMessageSender = PeerMessageSender(client)

  lazy val client: P2PClient = {
    val peerMessageReceiver =
      PeerMessageReceiver.newReceiver(node = node, peer = peer)
    P2PClient(context = system,
              peer = peer,
              peerMessageReceiver = peerMessageReceiver,
              onReconnect = node.sync,
              maxReconnectionTries = if (reconnect) 16 else 1)
  }

  private var _serviceIdentifier: Option[ServiceIdentifier] = None

  def serviceIdentifier: ServiceIdentifier = {
    _serviceIdentifier.getOrElse(
      throw new RuntimeException("Service identifier not initialized"))
  }

  def setServiceIdentifier(serviceIdentifier: ServiceIdentifier): Unit = {
    _serviceIdentifier = Some(serviceIdentifier)
  }
}

/**  This a base trait for various kinds of nodes. It contains house keeping methods required for all nodes.
  */
trait Node extends NodeApi with ChainQueryApi with P2PLogger {

  implicit def system: ActorSystem

  implicit def nodeAppConfig: NodeAppConfig

  implicit def chainAppConfig: ChainAppConfig

  implicit def executionContext: ExecutionContext = system.dispatcher

  //adding peers from config, db and dns seeds
  private def getPeersFromDnsSeeds: Vector[Peer] = {
    val dnsSeeds = nodeAppConfig.network.dnsSeeds
    val addresses = dnsSeeds
      .flatMap(seed => {
        try {
          InetAddress
            .getAllByName(seed)
        } catch {
          case _: UnknownHostException =>
            logger.info(s"DNS seed $seed is unavailable")
            Vector()
        }
      })
      .distinct
      .filter(_.isReachable(500))
      .map(_.getHostAddress)
    val inetSockets = addresses.map(
      NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
    val peers =
      inetSockets.map(Peer.fromSocket((_), nodeAppConfig.socks5ProxyParams))
    peers.toVector
  }

  private def getPeersFromDb: Future[Vector[Peer]] = {
    val addressesF =
      PeerDAO().findAll().map(_.map(p => new String(p.address.toArray)))
    val peersF = addressesF.map { addresses =>
      val inetSockets = addresses.map(
        NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
      // todo use saved port instead of default port
      val peers =
        inetSockets.map(Peer.fromSocket((_), nodeAppConfig.socks5ProxyParams))
      peers
    }
    peersF
  }

  private def getPeersFromConf: Vector[Peer] = {
    val addresses = nodeAppConfig.peers
    val inetSockets = addresses.map(
      NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
    val peers =
      inetSockets.map(Peer.fromSocket((_), nodeAppConfig.socks5ProxyParams))
    logger.info(peers)
    peers
  }

  def getPeers: Future[Unit] = {
    val peersFromConfig = getPeersFromConf
    lazy val peersFromDbF = getPeersFromDb
    lazy val peersFromSeeds = getPeersFromDnsSeeds

    val allF = for {
      peersFromDb <- peersFromDbF
    } yield {
      logger.info(s"db peers $peersFromDb")
      val maxPeers = 1
      var ret: Vector[Peer] = Vector()
      //choosing "maxPeers" no of elements from lists randomly in the order of peersFromConf, peersFromDB, peersFromSeed
      ret = ret ++ Random.shuffle(peersFromConfig).take(maxPeers)
      ret = ret ++ Random
        .shuffle(peersFromDb.diff(ret))
        .take(maxPeers - ret.length)
      //dns seeds won't be used if they are not required as startup time greatly increases because of them
      if (maxPeers - ret.length > 0)
        ret = ret ++ Random
          .shuffle(peersFromSeeds.diff(ret))
          .take(maxPeers - ret.length)
      logger.info(s"Selected peers: $ret")
      ret
    }
    for {
      all <- allF
    } yield all.foreach(addPeer(_, reconnect = false))
  }

  def peers: Vector[Peer] = peerData.keys.toVector

  def addPeer(peer: Peer, reconnect: Boolean = true): Unit = {
    if (!_peerData.contains(peer)) {
      _peerData.put(peer, PeerData(peer, this, reconnect))
    }
    ()
  }

  def removePeer(peer: Peer): Unit = {
    if (_peerData.contains(peer)) {
      _peerData.remove(peer)
    }
    ()
  }

  private val _peerData: mutable.Map[Peer, PeerData] =
    mutable.Map.empty

  def peerData: Map[Peer, PeerData] = _peerData.toMap

  def randomPeerMsgSenderWithService(
      f: ServiceIdentifier => Boolean): PeerMessageSender = {
    val filteredPeers = peerData
      .filter(p => f(p._2.serviceIdentifier))
      .keys
      .toVector
    if (filteredPeers.isEmpty)
      throw new RuntimeException("No peers supporting compact filters!")
    val peer = filteredPeers(Random.nextInt(filteredPeers.length))
    peerMsgSenders
      .find(_.client.peer == peer)
      .getOrElse(throw new RuntimeException("This should not happen."))
  }

  def randomPeerMsgSenderWithCompactFilters: PeerMessageSender = {
    randomPeerMsgSenderWithService(_.nodeCompactFilters)
  }

  def randomPeerMsgSender: PeerMessageSender = {
    peerMsgSenders(Random.nextInt(peerMsgSenders.length))
  }

  /** The current data message handler.
    * It should be noted that the dataMessageHandler contains
    * chainstate. When we update with a new chainstate, we need to
    * make sure we update the [[DataMessageHandler]] via [[updateDataMessageHandler()]]
    * to make sure we don't corrupt our chainstate cache
    */
  def getDataMessageHandler: DataMessageHandler

  def nodeCallbacks: NodeCallbacks = nodeAppConfig.nodeCallbacks

  lazy val txDAO: BroadcastAbleTransactionDAO = BroadcastAbleTransactionDAO()

  def updateDataMessageHandler(dataMessageHandler: DataMessageHandler): Node

  /** This is constructing a chain api from disk every time we call this method
    * This involves database calls which can be slow and expensive to construct
    * our [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
    */
  def chainApiFromDb()(implicit
      executionContext: ExecutionContext): Future[ChainHandlerCached] = {
    ChainHandlerCached.fromDatabase(BlockHeaderDAO(),
                                    CompactFilterHeaderDAO(),
                                    CompactFilterDAO())
  }

  /** Unlike our chain api, this is cached inside our node
    * object. Internally in [[org.bitcoins.node.networking.P2PClient p2p client]] you will see that
    * the [[ChainApi chain api]] is updated inside of the p2p client
    */
  def clients: Vector[P2PClient] = peerData.values.map(_.client).toVector

  def peerMsgSenders: Vector[PeerMessageSender] =
    peerData.values.map(_.peerMessageSender).toVector

  /** Sends the given P2P to our peer.
    * This method is useful for playing around
    * with P2P messages, therefore marked as
    * `private[node]`.
    */
  def send(msg: NetworkPayload, idx: Int): Future[Unit] = {
    peerMsgSenders(idx).sendMsg(msg)
  }

  /** Checks if we have a tcp connection with our peer */
  def isConnected(idx: Int): Future[Boolean] = peerMsgSenders(idx).isConnected()

  /** Checks if we are fully initialized with our peer and have executed the handshake
    * This means we can now send arbitrary messages to our peer
    *
    * @return
    */
  def isInitialized(idx: Int): Future[Boolean] =
    peerMsgSenders(idx).isInitialized()

  def isDisconnected(idx: Int): Future[Boolean] =
    peerMsgSenders(idx).isDisconnected()

  private def createInDbIfBlockFilterPeer(peer: Peer): Future[Unit] = {
    if (peerData(peer).serviceIdentifier.nodeCompactFilters) {
      logger.info(s"Our peer=$peer has been initialized")
      val addrBytes = peer.socket.getAddress.getAddress
      val networkByte =
        if (addrBytes.length == AddrV2Message.IPV4_ADDR_LENGTH) {
          AddrV2Message.IPV4_NETWORK_BYTE
        } else if (addrBytes.length == AddrV2Message.IPV6_ADDR_LENGTH) {
          AddrV2Message.IPV6_NETWORK_BYTE
        } else // todo might be tor, how to handle?
          throw new IllegalArgumentException("Unknown peer network")
      PeerDAO()
        .upsertPeer(ByteVector(addrBytes), peer.socket.getPort, networkByte)
        .map(_ => ())
    } else {
      logger.info(
        s"Our peer=$peer does not support compact filters. Disconnecting.")
      PeerDAO().deleteByKey(
        s"${peer.socket.getHostString}:${peer.socket.getPort}")
      peerData(peer).peerMessageSender.disconnect()
    }
  }

  def createInDbIfBlockFilterPeer(
      networkAddress: NetworkIpAddress): Future[Unit] = {
    if (networkAddress.services.nodeCompactFilters) {
      val (networkByte, bytes) = Try(networkAddress.address.ipv4Bytes) match {
        case Failure(_) =>
          (AddrV2Message.IPV6_NETWORK_BYTE, networkAddress.address.bytes)
        case Success(ipv4Bytes) =>
          (AddrV2Message.IPV4_NETWORK_BYTE, ipv4Bytes)
      }

      logger.info(s"Peer from addr: $bytes supports compact filters.")
      PeerDAO().upsertPeer(bytes, networkAddress.port, networkByte).map(_ => ())
    } else Future.unit
  }

  def createInDbIfBlockFilterPeer(addr: IPv4AddrV2Message): Future[Unit] = {
    val serviceIdentifier = ServiceIdentifier.fromBytes(addr.services.bytes)
    if (serviceIdentifier.nodeCompactFilters) {
      logger.info(s"Peer from addr: ${addr.addr} supports compact filters.")
      PeerDAO()
        .upsertPeer(addr.addrBytes, addr.port.toInt, addr.networkId)
        .map(_ => ())
    } else Future.unit
  }

  private def initializePeer(peer: Peer): Future[Unit] = {
    peerData(peer).peerMessageSender.connect()
    val isInitializedF =
      for {
        _ <- AsyncUtil
          .retryUntilSatisfiedF(
            () => peerData(peer).peerMessageSender.isInitialized(),
            maxTries = 50,
            interval = 250.millis)
          .recover { case NonFatal(_) =>
            logger.info(s"Failed to initialize $peer")
            removePeer(peer)
          }
      } yield ()
    isInitializedF.map { _ =>
      if (peerData.contains(peer)) {
        peerData(peer).peerMessageSender.sendGetAddrMessage()
        nodeAppConfig.nodeType match {
          case NodeType.NeutrinoNode => createInDbIfBlockFilterPeer(peer)
          case NodeType.SpvNode      =>
          case NodeType.BitcoindBackend =>
            throw new RuntimeException("Node cannot be BitcoindBackend")
          case NodeType.FullNode =>
        }
      }
    }
    isInitializedF
  }

  /** Starts our node */
  def start(): Future[Node] = {
    logger.info("Starting node")
    val start = System.currentTimeMillis()

    val startConfsF = for {
      _ <- chainAppConfig.start()
      _ <- nodeAppConfig.start()
    } yield ()

    val chainApiF = startConfsF.flatMap(_ => chainApiFromDb())

    val startNodeF = for {
      _ <- getPeers
      _ <- Future.sequence(peers.map(initializePeer))
    } yield {
      logger.info(s"Our node has been full started. It took=${System
        .currentTimeMillis() - start}ms")
      this
    }

    val bestHashF = chainApiF.flatMap(_.getBestBlockHash())
    val bestHeightF = chainApiF.flatMap(_.getBestHashBlockHeight())
    val filterHeaderCountF = chainApiF.flatMap(_.getFilterHeaderCount())
    val filterCountF = chainApiF.flatMap(_.getFilterCount())

    for {
      _ <- startConfsF
      node <- startNodeF

      _ = logger.trace("Fetching node starting point")
      bestHash <- bestHashF
      bestHeight <- bestHeightF
      filterHeaderCount <- filterHeaderCountF
      filterCount <- filterCountF
    } yield {
      logger.info(
        s"Started node, best block hash ${bestHash.hex} at height $bestHeight, with $filterHeaderCount filter headers and $filterCount filters")
      node
    }
  }

  /** Stops our node */
  def stop(): Future[Node] = {
    logger.info(s"Stopping node")

    val disconnectFs = peerMsgSenders.map(_.disconnect())

    val disconnectF = for {
      disconnect <- Future.sequence(disconnectFs)
      _ <- nodeAppConfig.stop()
    } yield disconnect

    def isAllDisconnectedF: Future[Boolean] = {
      val connF = peerMsgSenders.map(_.isDisconnected())
      val res = Future.sequence(connF).map(_.forall(_ == true))
      res
    }

    val start = System.currentTimeMillis()
    val isStoppedF = disconnectF.flatMap { _ =>
      logger.info(s"Awaiting disconnect")
      //25 seconds to disconnect
      AsyncUtil.retryUntilSatisfiedF(() => isAllDisconnectedF, 500.millis)
    }

    isStoppedF.failed.foreach { e =>
      logger.warn(s"Cannot stop node", e)
    }

    isStoppedF.map { _ =>
      logger.info(
        s"Node stopped! It took=${System.currentTimeMillis() - start}ms")
      this
    }
  }

  /** Starts to sync our node with our peer
    * If our local best block hash is the same as our peers
    * we will not sync, otherwise we will keep syncing
    * until our best block hashes match up
    *
    * @return
    */
  def sync(): Future[Unit] = {
    val blockchainsF =
      BlockHeaderDAO()(executionContext, chainAppConfig).getBlockchains()
    for {
      chainApi <- chainApiFromDb()
      header <- chainApi.getBestBlockHeader()
      blockchains <- blockchainsF

      // Get all of our cached headers in case of a reorg
      cachedHeaders = blockchains.flatMap(_.headers).map(_.hashBE.flip)
      _ <- randomPeerMsgSender.sendGetHeadersMessage(cachedHeaders)
    } yield {
      logger.info(
        s"Starting sync node, height=${header.height} hash=${header.hashBE}")
    }
  }

  /** Broadcasts the given transaction over the P2P network */
  override def broadcastTransactions(
      transactions: Vector[Transaction]): Future[Unit] = {
    val broadcastTxDbs = transactions.map(tx => BroadcastAbleTransaction(tx))

    val addToDbF = txDAO.upsertAll(broadcastTxDbs)

    val txIds = transactions.map(_.txIdBE.hex)

    addToDbF.onComplete {
      case Failure(exception) =>
        logger.error(s"Error when writing broadcastable TXs to DB", exception)
      case Success(written) =>
        logger.debug(
          s"Wrote tx=${written.map(_.transaction.txIdBE.hex)} to broadcastable table")
    }

    for {
      _ <- addToDbF

      connected <- isConnected(0)

      res <- {
        if (connected) {
          logger.info(s"Sending out tx message for tx=$txIds")
          peerMsgSenders(0).sendInventoryMessage(transactions: _*)
        } else {
          Future.failed(new RuntimeException(
            s"Error broadcasting transaction $txIds, peer is disconnected ${peers(0)}"))
        }
      }
    } yield res
  }

  /** Fetches the given blocks from the peers and calls the appropriate [[callbacks]] when done.
    */
  override def downloadBlocks(
      blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = {
    if (blockHashes.isEmpty) {
      Future.unit
    } else {
      peerMsgSenders(0).sendGetDataMessage(TypeIdentifier.MsgWitnessBlock,
                                           blockHashes: _*)
    }
  }

  /** Gets the height of the given block */
  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
    chainApiFromDb().flatMap(_.getBlockHeight(blockHash))

  /** Gets the hash of the block that is what we consider "best" */
  override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
    chainApiFromDb().flatMap(_.getBestBlockHash())

  /** Gets number of confirmations for the given block hash */
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] =
    chainApiFromDb().flatMap(_.getNumberOfConfirmations(blockHashOpt))

  override def epochSecondToBlockHeight(time: Long): Future[Int] =
    chainApiFromDb().flatMap(_.epochSecondToBlockHeight(time))

}
