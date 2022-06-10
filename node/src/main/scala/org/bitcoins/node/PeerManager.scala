package org.bitcoins.node

import akka.actor.{ActorSystem, Cancellable, PoisonPill}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.p2p.{AddrV2Message, ServiceIdentifier}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageSender
import scodec.bits.ByteVector

import java.net.{InetAddress, UnknownHostException}
import java.time.Duration
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Random
import scala.util.control.NonFatal

case class PeerManager(node: Node, configPeers: Vector[Peer] = Vector.empty)(
    implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig)
    extends P2PLogger {

  /* peers are stored across _peerData and _testPeerData.
  _peerData has all the peers that the node is actually using in its operation.
  _testPeerData is for temporarily keeping peer information for peer discovery (we do not store every peer rather initialize
  and then verify that it can be reached and supports compact filters).
   */
  private val _peerData: mutable.Map[Peer, PeerData] = mutable.Map.empty

  private val _testPeerData: mutable.Map[Peer, PeerData] = mutable.Map.empty

  def peerData: Map[Peer, PeerData] = _peerData.toMap

  def testPeerData: Map[Peer, PeerData] = _testPeerData.toMap

  def connectedPeerCount: Int = peerData.size

  //stack to store peers to connect to as part of peer discovery
  //Why stack? Peers are added as per order resources, db, config at the start so in issue of order there. Only during
  //node operation, it is intended to try peers from addr messages above all hence a stack to have them first.
  //might want to change to priority queue to make order enforced (?)
  val peerDiscoveryStack: mutable.Stack[Peer] = mutable.Stack.empty[Peer]

  val maxPeerSearchCount =
    1000 //number of peers in db at which we stop peer discovery

  lazy val peerConnectionScheduler: Cancellable =
    system.scheduler.scheduleWithFixedDelay(initialDelay = 10.seconds,
                                            delay = 10.seconds) {
      new Runnable() {
        override def run(): Unit = {
          val peersInDbCountF = PeerDAO().count()
          peersInDbCountF.map(cnt =>
            if (cnt > maxPeerSearchCount) peerConnectionScheduler.cancel())

          if (peerDiscoveryStack.size < 16) {
            peerDiscoveryStack.pushAll(getPeersFromDnsSeeds)
          }

          val peers = for { _ <- 1 to 16 } yield peerDiscoveryStack.pop()
          peers.foreach(peer => {
            addTestPeer(peer)
          })
        }
      }
    }

  /** moves a peer from [[_testPeerData]] to [[_peerData]]
    * this operation makes the node permanently keep connection with the peer and
    * use it for node operation
    */
  def setPeerForUse(peer: Peer): Future[Unit] = {
    require(testPeerData.contains(peer), "Unknown peer marked as usable")
    _peerData(peer) = peerDataOf(peer)
    logger.info(
      s"Connected to peer $peer. Connected peer count $connectedPeerCount")
    _testPeerData.remove(peer)
    peerData(peer).peerMessageSender.sendGetAddrMessage()
  }

  /** disconnects the currently used peer for sync and uses a new one */
  def useNewPeer(): Future[Unit] = {
    val oldPeer: Peer = peerUsedForSync.getOrElse(
      throw new RuntimeException("No old peer set for sync yet."))

    peerData(oldPeer).peerMessageSender.client.actor ! PoisonPill
    _peerData.remove(oldPeer)

    val setNewPeerF = AsyncUtil
      .retryUntilSatisfied(connectedPeerCount > 0,
                           interval = 1.seconds,
                           maxTries = 600)
      .map { _ =>
        val peer = randomPeerWithService(_.nodeCompactFilters)
        _reconnectSyncCount = 0
        setPeerUsedForSync(peer)
      }
    for {
      _ <- setNewPeerF
      _ <- node.sync()
    } yield {
      logger.info(s"Using new peer ${peerUsedForSync.get} for sync")
    }
  }

  //for reconnect, we would only want to call node.sync if the peer reconnected is the one that was
  //already syncing. So storing that.
  private var _peerUsedForSync: Option[Peer] = None
  //some peers can be connected and initialized but then they disconnect us immediately.
  //such a peer cannot be used for node operation as we get stuck in an infinite reconnect disconnect loop.
  private var _reconnectSyncCount: Int = 0

  def incrementReconnectCount(): Unit = _reconnectSyncCount += 1

  def reconnectCount: Int = _reconnectSyncCount

  def peerUsedForSync: Option[Peer] = _peerUsedForSync

  def setPeerUsedForSync(peer: Peer): Unit = {
    _peerUsedForSync = Some(peer)
  }

  def peers: Vector[Peer] = peerData.keys.toVector

  def peerMsgSenders: Vector[PeerMessageSender] =
    peerData.values
      .map(_.peerMessageSender)
      .toVector

  def clients: Vector[P2PClient] = peerData.values.map(_.client).toVector

  /** Returns peers by querying each dns seed once. These will be IPv4 addresses. */
  private def getPeersFromDnsSeeds: Vector[Peer] = {
    val dnsSeeds = nodeAppConfig.network.dnsSeeds
    val addresses = dnsSeeds
      .flatMap(seed => {
        try {
          InetAddress
            .getAllByName(seed)
        } catch {
          case _: UnknownHostException =>
            logger.debug(s"DNS seed $seed is unavailable.")
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

  /** Returns peers from hardcoded addresses taken from https://github.com/bitcoin/bitcoin/blob/master/contrib/seeds/nodes_main.txt */
  private def getPeersFromResources: Vector[Peer] = {
    val source = Source.fromURL(getClass.getResource("/hardcoded-peers.txt"))
    val addresses = source
      .getLines()
      .toVector
      .filter(nodeAppConfig.torConf.enabled || !_.contains(".onion"))
    val inetSockets = addresses.map(
      NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
    val peers =
      inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
    peers
  }

  /** Returns all peers stored in database */
  private def getPeersFromDb: Future[Vector[Peer]] = {
    val addressesF: Future[Vector[PeerDb]] =
      PeerDAO().findAllWithTorFilter(nodeAppConfig.torConf.enabled)
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

  /** Returns peers from bitcoin-s.config file unless peers are supplied as an argument to [[PeerManager]] in which
    * case it returns those.
    */
  def getPeersFromConfig: Vector[Peer] = {
    if (configPeers.nonEmpty) {
      configPeers
    } else {
      val addresses = nodeAppConfig.peers.filter(
        nodeAppConfig.torConf.enabled || !_.contains(".onion"))
      val inetSockets = addresses.map(
        NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
      val peers =
        inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
      peers
    }
  }

  /** initial setup for peer discovery. Does the following:
    * load peers from resources into discovery stack
    * starts connecting with config and db peers.
    */
  def start: Future[Unit] = {
    val peersFromConfig = getPeersFromConfig
    val peersFromResources = getPeersFromResources

    for {
      peersFromDb <- getPeersFromDb
    } yield {
      peersFromConfig.take(1).foreach(addTestPeer)
      peerDiscoveryStack.pushAll(peersFromConfig.drop(1))
      peerDiscoveryStack.pushAll(Random.shuffle(peersFromResources))
      peerDiscoveryStack.pushAll(Random.shuffle(peersFromDb))
      peerConnectionScheduler //start scheduler
      ()
    }
  }

  def stop: Future[Unit] = {
    peerConnectionScheduler.cancel()
    peerDiscoveryStack.clear()
    val removeTestPeersF =
      Future.sequence(testPeerData.keys.map(removeTestPeer))
    for {
      _ <- removeTestPeersF
      _ <- Future.sequence(peerMsgSenders.map(_.disconnect()))
      _ <- AsyncUtil.retryUntilSatisfiedF(
        () =>
          Future
            .sequence(peerMsgSenders.map(_.isDisconnected()))
            .map(_.forall(_ == true)),
        500.millis)
      _ <- Future.sequence(peers.map(removePeer))
    } yield ()
  }

  /** creates and initialises a new test peer */
  def addTestPeer(peer: Peer): Unit = {
    if (!_testPeerData.contains(peer)) {
      _testPeerData.put(peer, PeerData(peer, node))
      initializePeer(peer)
    } else logger.debug(s"Peer $peer already added.")
    ()
  }

  def removeTestPeer(peer: Peer): Future[Unit] = {
    if (_testPeerData.contains(peer)) {
      testPeerData(peer).peerMessageSender.client.actor.!(PoisonPill)
      _testPeerData.remove(peer)
      Future.unit
    } else {
      logger.debug(s"Key $peer not found in peerData")
      Future.unit
    }
  }

  def removePeer(peer: Peer): Future[Unit] = {
    if (_peerData.contains(peer)) {
      _peerData(peer).peerMessageSender.client.actor.!(PoisonPill)
      _peerData.remove(peer)
      Future.unit
    } else {
      logger.debug(s"Key $peer not found in peerData")
      Future.unit
    }
  }

  def randomPeerWithService(f: ServiceIdentifier => Boolean): Peer = {

    //TODO:
    // this should ultimately be random but in the context of this pr, which does not have the full functionality
    // is not done yet, in a lot of places the first peer is taken to match the behaviour of when there is just one peer

    val filteredPeers =
      peerData.filter(p => f(p._2.serviceIdentifier)).toVector
    if (filteredPeers.isEmpty) {
      throw new RuntimeException("No peers supporting compact filters!")
    }
//    val randomPeer = filteredPeers(Random.nextInt(filteredPeers.length))
    val randomPeer = filteredPeers(0)
    randomPeer._1
  }

  def createInDb(peer: Peer): Future[PeerDb] = {
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
      case unknownSize =>
        throw new IllegalArgumentException(
          s"Unsupported address type of size $unknownSize bytes")
    }
    PeerDAO()
      .upsertPeer(ByteVector(addrBytes), peer.socket.getPort, networkByte)
  }

  //makes it more readable, compare peerManager.peerData(peer) vs peerManager.peerDataOf(peer) as peer is used thrice
  //in a simple statement
  /** get [[PeerData]] for a [[Peer]] */
  def peerDataOf(peer: Peer): PeerData = {
    peerData.getOrElse(peer,
                       testPeerData.getOrElse(
                         peer,
                         throw new RuntimeException(s"Key $peer not found")))
  }

  def awaitPeerWithService(
      f: ServiceIdentifier => Boolean,
      timeout: Duration): Future[Unit] = {
    logger.info("Waiting for peer connection")
    val ret = AsyncUtil
      .retryUntilSatisfied({
                             peerData.exists(x => f(x._2.serviceIdentifier))
                           },
                           interval = 1.seconds,
                           maxTries = timeout.getSeconds.toInt)
      .recover {
        case _: AsyncUtil.RpcRetryException =>
          throw new RuntimeException("No supported peers found!")
        case unknown: Throwable => throw unknown
      }
      .map(_ => logger.info("Connected to peer. Starting sync."))

    ret
  }

  def initializePeer(peer: Peer): Future[Unit] = {
    peerDataOf(peer).peerMessageSender.connect()
    val isInitializedF =
      for {
        _ <- AsyncUtil
          .retryUntilSatisfiedF(
            () => peerDataOf(peer).peerMessageSender.isInitialized(),
            maxTries = 10,
            interval = 1.seconds)
          .recover { case NonFatal(_) =>
            removeTestPeer(peer);
          }
      } yield ()
    isInitializedF
  }
}
