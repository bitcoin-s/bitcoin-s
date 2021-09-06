package org.bitcoins.node

import akka.actor.{ActorSystem, Cancellable}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.blockchain.ChainHandlerCached
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.chain._
import org.bitcoins.core.api.node.NodeApi
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

import java.util.concurrent.TimeUnit
import java.net.{InetAddress, UnknownHostException}
import scala.concurrent.duration.Duration
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

case class PeerData(
    peer: Peer,
    node: Node,
    var keepConnection: Boolean
)(implicit system: ActorSystem, nodeAppConfig: NodeAppConfig) {

  lazy val peerMessageSender: PeerMessageSender = PeerMessageSender(client)

  lazy val client: P2PClient = {
    val peerMessageReceiver =
      PeerMessageReceiver.newReceiver(node = node, peer = peer)
    P2PClient(
      context = system,
      peer = peer,
      peerMessageReceiver = peerMessageReceiver,
      onReconnect = if (keepConnection) node.sync else () => { Future.unit },
      maxReconnectionTries = if (keepConnection) 16 else 1
    )
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

  val peersToCheckStack: mutable.Stack[Peer] = mutable.Stack.empty[Peer]

  private def getPeersFromDnsSeeds: Vector[Peer] = {
    val dnsSeeds = nodeAppConfig.network.dnsSeeds
    val addresses = dnsSeeds
      .flatMap(seed => {
        try {
          InetAddress
            .getAllByName(seed)
        } catch {
          case _: UnknownHostException =>
            logger.debug(s"DNS seed $seed is unavailable")
            Vector()
        }
      })
      .distinct
      .filter(_.isReachable(500))
      .map(_.getHostAddress)
    val inetSockets = addresses.map(
      NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
    val peers =
      inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
    peers.toVector
  }

  private def getPeersFromDb: Future[Vector[Peer]] = {
    val addressesF: Future[Vector[PeerDB]] =
      PeerDAO().findAll()
    val peersF = addressesF.map { addresses =>
      val inetSockets = addresses.map(a => {
        NetworkUtil.parseInetSocketAddress(a.address, a.port)
      })
      val peers =
        inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
      peers
    }
    peersF
  }

  def getPeersFromConf: Vector[Peer] = {
    val addresses = nodeAppConfig.peers
    val inetSockets = addresses.map(
      NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
    val peers =
      inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
    peers
  }

  lazy val peerConnectionScheduler: Cancellable =
    system.scheduler.scheduleWithFixedDelay(initialDelay =
                                              Duration(10, TimeUnit.SECONDS),
                                            delay =
                                              Duration(5, TimeUnit.SECONDS)) {
      new Runnable() {
        override def run(): Unit = {
          if (peersToCheckStack.size < 10)
            peersToCheckStack.pushAll(getPeersFromDnsSeeds)
          val peers = for { _ <- 0 to 9 } yield peersToCheckStack.pop()
          peers.foreach(peer => {
            addPeer(peer, keepConnection = false)
            initializePeer(peer)
          })
        }
      }
    }

  def getPeers: Future[Unit] = {
    val peersFromConfig = getPeersFromConf
    lazy val peersFromDbF = getPeersFromDb

    val allF = for {
      peersFromDb <- peersFromDbF
    } yield {
      val ret = Vector.newBuilder[Peer]
      //choosing "maxPeers" no of elements from lists randomly in the order of peersFromConf, peersFromDB
      ret ++= Random.shuffle(peersFromConfig).take(maxConnectedPeers)
      if (maxConnectedPeers - ret.result().size > 0)
        ret ++= Random
          .shuffle(peersFromDb.diff(ret.result()))
          .take(maxConnectedPeers - ret.result().size)
      ret.result().foreach(addPeer(_, keepConnection = true))
    }
    allF
  }

  def peers: Vector[Peer] = peerData.filter(_._2.keepConnection).keys.toVector

  def addPeer(
      peer: Peer,
      keepConnection: Boolean = true
  ): Unit = {
    if (!_peerData.contains(peer)) {
      _peerData.put(peer, PeerData(peer, this, keepConnection))
    } else logger.debug(s"Peer $peer already added.")
    ()
  }

  def removePeer(peer: Peer): Unit = {
    if (_peerData.contains(peer)) {
      peerData(peer).peerMessageSender.disconnect()
      _peerData.remove(peer)
    } else logger.debug(s"Peer $peer does not exist in set")
    ()
  }

  private val _peerData: mutable.Map[Peer, PeerData] =
    mutable.Map.empty

  def peerData: Map[Peer, PeerData] = _peerData.toMap

  def randomPeerMsgSenderWithService(
      f: ServiceIdentifier => Boolean): PeerMessageSender = {
    val filteredPeers = peerData
      .filter(p => f(p._2.serviceIdentifier) && p._2.keepConnection)
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

  //todo think of a better name
  def handlePeerGossipMessage(message: Any): Unit

  /** Unlike our chain api, this is cached inside our node
    * object. Internally in [[org.bitcoins.node.networking.P2PClient p2p client]] you will see that
    * the [[ChainApi chain api]] is updated inside of the p2p client
    */
  def clients: Vector[P2PClient] =
    peerData.filter(_._2.keepConnection).values.map(_.client).toVector

  def peerMsgSenders: Vector[PeerMessageSender] =
    peerData
      .filter(_._2.keepConnection)
      .values
      .map(_.peerMessageSender)
      .toVector

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

  def connectedPeersCount: Int = peerData.count(_._2.keepConnection)
  val maxConnectedPeers = 4

  def onPeerInitialization(peer: Peer): Future[Unit]

  def initializePeer(peer: Peer): Future[Unit] = {
    peerData(peer).peerMessageSender.connect()
    val isInitializedF =
      for {
        _ <- AsyncUtil
          .retryUntilSatisfiedF(
            () => peerData(peer).peerMessageSender.isInitialized(),
            maxTries = 50,
            interval = 250.millis)
          .recover { case NonFatal(_) =>
            logger.debug(s"Failed to initialize $peer")
            removePeer(peer)
          }
      } yield ()
    for {
      _ <- isInitializedF
      _ <- onPeerInitialization(peer)
    } yield ()
  }

  /** Starts our node */
  def start(): Future[Node] = {
    logger.info("Starting node")
    peerConnectionScheduler
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
