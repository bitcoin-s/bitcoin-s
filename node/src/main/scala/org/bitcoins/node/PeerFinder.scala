package org.bitcoins.node

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.util.{NetworkUtil, StartStopAsync}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}

import java.net.{InetAddress, UnknownHostException}
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Random

case class PeerFinder(
    paramPeers: Vector[Peer],
    node: NeutrinoNode,
    skipPeers: () => Vector[Peer],
    supervisor: ActorRef)(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig)
    extends StartStopAsync[PeerFinder]
    with P2PLogger {

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

  private def getPeersFromParam: Vector[Peer] = {
    logger.debug(s"Param peers: $paramPeers")
    paramPeers
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
  private val _peerData: mutable.Map[Peer, PeerData] = mutable.Map.empty

  private val _peersToTry: PeerStack = PeerStack()

  val maxPeerSearchCount: Int = 1000

  def initialDelay: FiniteDuration = {
    if (getPeersFromConfig.isEmpty && getPeersFromParam.isEmpty) 0.seconds
    else nodeAppConfig.tryNextPeersInterval
  }

  private lazy val peerConnectionScheduler: Cancellable =
    system.scheduler.scheduleWithFixedDelay(
      initialDelay = initialDelay,
      delay = nodeAppConfig.tryNextPeersInterval) {
      new Runnable() {
        override def run(): Unit = {
          logger.debug(s"Cache size: ${_peerData.size}. ${_peerData.keys}")
          if (_peersToTry.size < 32)
            _peersToTry.pushAll(getPeersFromDnsSeeds)

          val peers = (for { _ <- 1 to 32 } yield _peersToTry.pop()).distinct
            .filterNot(p => skipPeers().contains(p) || _peerData.contains(p))

          logger.debug(s"Trying next set of peers $peers")
          peers.foreach(tryPeer)
        }
      }
    }

  override def start(): Future[PeerFinder] = {
    logger.debug(s"Starting PeerFinder")

    (getPeersFromParam ++ getPeersFromConfig).distinct.foreach(tryPeer)

    if (nodeAppConfig.enablePeerDiscovery) {
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
      Future.successful(this)
    }
  }

  override def stop(): Future[PeerFinder] = {
    //stop scheduler
    peerConnectionScheduler.cancel()
    //delete try queue
    _peersToTry.clear()

    _peerData.foreach(_._2.client.close())

    AsyncUtil
      .retryUntilSatisfied(_peerData.isEmpty,
                           interval = 1.seconds,
                           maxTries = 10)
      .map(_ => this)
  }

  /** creates and initialises a new test peer */
  def tryPeer(peer: Peer): Unit = {
    _peerData.put(peer, PeerData(peer, node, supervisor))
    _peerData(peer).peerMessageSender.connect()
  }

  def removePeer(peer: Peer): Unit = {
    logger.debug(s"Removing peer $peer")
    _peerData.remove(peer)
    ()
  }

  def setServiceIdentifier(
      peer: Peer,
      serviceIdentifier: ServiceIdentifier): Unit = {
    _peerData(peer).setServiceIdentifier(serviceIdentifier)
  }

  def popFromCache(peer: Peer): Option[PeerData] = {
    if (_peerData.contains(peer))
      _peerData.remove(peer)
    else {
      logger.debug(s"removeFromCache: $peer not found in peerData")
      None
    }
  }

  def hasPeer(peer: Peer): Boolean = {
    _peerData.contains(peer)
  }

  def getData(peer: Peer): PeerData = {
    assert(hasPeer(peer), "finder.getData called without any data on peer")
    _peerData(peer)
  }

  def addToTry(peers: Vector[Peer], priority: Int = 0): Unit = {
    _peersToTry.pushAll(peers, priority)
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
