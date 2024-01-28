package org.bitcoins.node

import akka.actor.{ActorSystem, Cancellable}
import akka.stream.scaladsl.{SourceQueue}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.Peer
import org.bitcoins.core.p2p.{ServiceIdentifier, VersionMessage}
import org.bitcoins.core.util.{NetworkUtil, StartStopAsync}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{PeerDAO, PeerDb}
import org.bitcoins.node.networking.peer.{
  ControlMessageHandler,
  PeerConnection,
  PeerMessageSender
}

import java.net.{InetAddress, UnknownHostException}
import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Random, Success}

case class PeerFinder(
    paramPeers: Vector[Peer],
    queue: SourceQueue[NodeStreamMessage],
    skipPeers: () => Set[Peer])(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig,
    chainAppConfig: ChainAppConfig)
    extends StartStopAsync[PeerFinder]
    with P2PLogger {

  private val isStarted: AtomicBoolean = new AtomicBoolean(false)

  val controlMessageHandler: ControlMessageHandler =
    ControlMessageHandler(this)

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
            Vector.empty
        }
      })
      .distinct
      .map(_.getHostAddress)
    stringsToPeers(addresses.toVector)
  }

  /** Returns peers from hardcoded addresses taken from https://github.com/bitcoin/bitcoin/blob/master/contrib/seeds/nodes_main.txt */
  private def getPeersFromResources: Vector[Peer] = {
    val source = Source.fromURL(getClass.getResource("/hardcoded-peers.txt"))
    val addresses = source
      .getLines()
      .toVector
      .filter(nodeAppConfig.torConf.enabled || !_.contains(".onion"))
    val peers = stringsToPeers(addresses)
    Random.shuffle(peers)
  }

  /** Returns tuple (non-filter peer, filter peers) from all peers stored in database */
  private def getPeersFromDb: Future[(Vector[Peer], Vector[Peer])] = {
    val dbF: Future[Vector[PeerDb]] =
      PeerDAO().findAllWithTorFilter(nodeAppConfig.torConf.enabled)

    val partitionF = dbF.map(_.partition(b =>
      !ServiceIdentifier.fromBytes(b.serviceBytes).nodeCompactFilters))

    def toPeers(peerDbs: Vector[PeerDb]): Vector[Peer] = {
      val inetSockets = peerDbs.map(a => {
        NetworkUtil.parseInetSocketAddress(a.address, a.port)
      })
      val peers =
        inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
      Random.shuffle(peers)
    }

    partitionF.map(p => (toPeers(p._1), toPeers(p._2)))
  }

  /** Returns peers from bitcoin-s.config file unless peers are supplied as an argument to [[PeerManager]] in which
    * case it returns those.
    */
  private def getPeersFromConfig: Vector[Peer] = {
    val addresses = nodeAppConfig.peers.filter(
      nodeAppConfig.torConf.enabled || !_.contains(".onion"))
    val peers = stringsToPeers(addresses)
    logger.debug(s"Config peers: $peers")
    peers
  }

  private def stringsToPeers(addresses: Vector[String]): Vector[Peer] = {
    val formatStrings = addresses.map { s =>
      //assumes strings are valid, todo: add util functions to check fully for different addresses
      if (s.count(_ == ':') > 1 && s(0) != '[') //ipv6
        "[" + s + "]"
      else s
    }
    val inetSockets = formatStrings.map(
      NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
    val peers =
      inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
    peers
  }

  //for the peers we try
  private val _peerData: mutable.Map[Peer, PeerData] = {
    mutable.Map.empty
  }

  private val _peersToTry: PeerStack = PeerStack()

  private val maxPeerSearchCount: Int = 8

  private val initialDelay: FiniteDuration = nodeAppConfig.tryPeersStartDelay

  private val isConnectionSchedulerRunning = new AtomicBoolean(false)

  private lazy val peerConnectionScheduler: Cancellable =
    system.scheduler.scheduleWithFixedDelay(
      initialDelay = initialDelay,
      delay = nodeAppConfig.tryNextPeersInterval) { () =>
      {
        logger.info(s"Running peerConnectionScheduler")
        if (isConnectionSchedulerRunning.compareAndSet(false, true)) {
          logger.info(s"Querying p2p network for peers...")
          logger.debug(s"Cache size: ${_peerData.size}. ${_peerData.keys}")
          if (_peersToTry.size < maxPeerSearchCount)
            _peersToTry.pushAll(getPeersFromDnsSeeds)

          //in case of less _peersToTry.size than maxPeerSearchCount
          val max = Math.min(maxPeerSearchCount, _peersToTry.size)
          val peers = (
            1.to(max)
              .map(_ => _peersToTry.pop()))
            .distinct
            .filterNot(p => skipPeers().contains(p) || _peerData.contains(p))

          logger.debug(s"Trying next set of peers $peers")
          val peersF = Future.traverse(peers)(tryPeer)
          peersF.onComplete {
            case Success(_) =>
              isConnectionSchedulerRunning.set(false)
            case Failure(err) =>
              isConnectionSchedulerRunning.set(false)
              logger.debug(
                s"Failed to connect to peers=$peers errMsg=${err.getMessage}")
          }
        } else {
          logger.warn(
            s"Previous connection scheduler is still running, skipping this run, it will run again in ${nodeAppConfig.tryNextPeersInterval}")
        }
      }
    }

  override def start(): Future[PeerFinder] = {
    logger.info(
      s"Starting PeerFinder initialDelay=${initialDelay} paramPeers=$paramPeers")
    val start = System.currentTimeMillis()
    isStarted.set(true)
    val peersToTry = (paramPeers ++ getPeersFromConfig).distinct
    _peersToTry.pushAll(peersToTry)

    val peerDiscoveryF = if (nodeAppConfig.enablePeerDiscovery) {
      val startedF = for {
        (dbNonCf, dbCf) <- getPeersFromDb
      } yield {
        _peersToTry.pushAll(getPeersFromDnsSeeds)
        _peersToTry.pushAll(getPeersFromResources)
        _peersToTry.pushAll(dbNonCf)
        _peersToTry.pushAll(dbCf, priority = 1)
        peerConnectionScheduler //start scheduler

        this
      }

      startedF
    } else {
      logger.info("Peer discovery disabled.")
      peerConnectionScheduler //start scheduler
      Future.successful(this)
    }

    for {
      peerFinder <- peerDiscoveryF
      _ = logger.info(
        s"Done starting PeerFinder, it took ${System.currentTimeMillis() - start}ms")
    } yield peerFinder
  }

  def reconnect(peer: Peer): Future[Unit] = {
    logger.info(s"Attempting to reconnect peer=$peer")
    if (isStarted.get) {
      tryToReconnectPeer(peer)
    } else {
      logger.error(
        s"Ignoring reconnect attempt to peer=$peer as PeerFinder is not started")
      Future.unit
    }
  }

  override def stop(): Future[PeerFinder] = {
    logger.info(s"Stopping PeerFinder")
    isStarted.set(false)
    //stop scheduler
    peerConnectionScheduler.cancel()
    //delete try queue
    _peersToTry.clear()

    val stopF = for {
      _ <- Future.traverse(_peerData.map(_._1))(removePeer(_))
      _ <- AsyncUtil
        .retryUntilSatisfied(_peerData.isEmpty,
                             interval = 1.seconds,
                             maxTries = 30)
    } yield {
      logger.info(s"Done stopping PeerFinder")
      this
    }

    stopF.failed.foreach { e =>
      logger.error(s"Failed to stop peer finder. Peers: ${_peerData.map(_._1)}",
                   e)
    }
    stopF
  }

  /** creates and initialises a new test peer */
  private def tryPeer(peer: Peer): Future[Unit] = {
    logger.debug(s"tryPeer=$peer")
    val peerConnection = PeerConnection(peer, queue)
    val peerMessageSender = PeerMessageSender(peerConnection)
    _peerData.put(peer, PersistentPeerData(peer, peerMessageSender))
    peerConnection.connect()
  }

  private def tryToReconnectPeer(peer: Peer): Future[Unit] = {
    val peerConnection = PeerConnection(peer, queue)
    val peerMessageSender = PeerMessageSender(peerConnection)
    _peerData.put(peer, PersistentPeerData(peer, peerMessageSender))
    peerConnection.reconnect()

  }

  def removePeer(peer: Peer): Future[Option[PeerData]] = {
    Future {
      logger.debug(s"Removing peer=$peer")
      _peerData.remove(peer) //peer must be a member of _peerData
    }
  }

  def setServiceIdentifier(
      peer: Peer,
      serviceIdentifier: ServiceIdentifier): Unit = {
    _peerData(peer).setServiceIdentifier(serviceIdentifier)
  }

  def popFromCache(peer: Peer): Option[PersistentPeerData] = {
    _peerData.get(peer) match {
      case Some(persistentPeerData: PersistentPeerData) =>
        _peerData.remove(peer)
        Some(persistentPeerData)
      case Some(_: AttemptToConnectPeerData) => None
      case None =>
        logger.debug(s"removeFromCache: $peer not found in peerData")
        None
    }
  }

  def hasPeer(peer: Peer): Boolean = {
    _peerData.contains(peer)
  }

  def getPeerData(peer: Peer): Option[PeerData] = {
    _peerData.get(peer)
  }

  def addToTry(peers: Vector[Peer], priority: Int = 0): Unit = {
    _peersToTry.pushAll(peers, priority)
  }

  def onVersionMessage(peer: Peer, versionMsg: VersionMessage): Unit = {
    if (hasPeer(peer)) {
      getPeerData(peer).get.setServiceIdentifier(versionMsg.services)
    } else {
      logger.warn(s"onVersionMessage called for unknown $peer")
    }
  }
}

case class PeerStack() {

  case class PeerOrdering(peer: Peer, priority: Int, id: Int)

  implicit def ordering: Ordering[PeerOrdering] =
    (x: PeerOrdering, y: PeerOrdering) => {
      if (x.priority != y.priority) x.priority.compare(y.priority)
      else x.id.compare(y.id)
    }

  private var id: Int = 0

  private val maxSize = 5000

  private val set: mutable.SortedSet[PeerOrdering] =
    mutable.SortedSet[PeerOrdering]().empty

  def push(peer: Peer, priority: Int = 0): Unit = {
    if (set.size == maxSize) {
      if (set.head.priority < priority) {
        set.remove(set.head)
        set.add(PeerOrdering(peer, priority, id))
        id += 1
      }
    } else {
      set.add(PeerOrdering(peer, priority, id))
      id += 1
    }
    ()
  }

  def pop(): Peer = {
    val res = set.last.peer
    set.remove(set.last)
    res
  }

  def size: Int = set.size

  def clear(): Unit = set.clear()

  def pushAll(peers: Vector[Peer], priority: Int = 0): Unit = {
    peers.foreach(push(_, priority))
  }
}
