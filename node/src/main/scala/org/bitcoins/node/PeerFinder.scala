package org.bitcoins.node

import akka.actor.{ActorSystem, Cancellable}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.p2p.ServiceIdentifier
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}

import java.net.{InetAddress, UnknownHostException}
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Random

object PeerSource extends Enumeration {
  type PeerSource = Value
  val Config, Db, Dns, Addr, Param = Value
}

case class PeerFinder(paramPeers: Vector[Peer], skipPeers: () => Vector[Peer])(
    implicit
    node: Node,
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig)
    extends P2PLogger {

  /** Returns peers by querying each dns seed once. These will be IPv4 addresses. */
  def getPeersFromDnsSeeds: Vector[Peer] = {
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
    stringsToPeers(addresses.toVector)
  }

  /** Returns peers from hardcoded addresses taken from https://github.com/bitcoin/bitcoin/blob/master/contrib/seeds/nodes_main.txt */
  def getPeersFromResources: Vector[Peer] = {
    val source = Source.fromURL(getClass.getResource("/hardcoded-peers.txt"))
    val addresses = source
      .getLines()
      .toVector
      .filter(nodeAppConfig.torConf.enabled || !_.contains(".onion"))
    val peers = stringsToPeers(addresses)
    Random.shuffle(peers)
  }

  /** Returns all peers stored in database */
  def getPeersFromDb: Future[Vector[Peer]] = {
    val dbF: Future[Vector[PeerDb]] =
      PeerDAO().findAllWithTorFilter(nodeAppConfig.torConf.enabled)
    val partitionF = dbF.map(_.partition(b =>
      ServiceIdentifier.fromBytes(b.serviceBytes).nodeCompactFilters))
    val addressesF =
      partitionF.map(x => Random.shuffle(x._1) ++ Random.shuffle(x._2))
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
    val inetSockets = addresses.map(
      NetworkUtil.parseInetSocketAddress(_, nodeAppConfig.network.port))
    val peers =
      inetSockets.map(Peer.fromSocket(_, nodeAppConfig.socks5ProxyParams))
    peers
  }

  //for the peers we try
  private val _peerData: mutable.Map[Peer, PeerData] = mutable.Map.empty

  private val _peersToTry: mutable.Stack[Peer] = mutable.Stack.empty

  val maxPeerSearchCount: Int = 1000

  val timeoutForTrying: FiniteDuration = 6.second

  private lazy val peerConnectionScheduler: Cancellable =
    system.scheduler.scheduleWithFixedDelay(initialDelay = timeoutForTrying,
                                            delay = 3 * timeoutForTrying) {
      new Runnable() {
        override def run(): Unit = {
          logger.debug(s"Cache size: ${_peerData.size}. ${_peerData.keys}")
          if (_peersToTry.size < 24)
            _peersToTry.pushAll(getPeersFromDnsSeeds)

          val peers = (for { _ <- 1 to 24 } yield _peersToTry.pop()).distinct
            .filterNot(skipPeers().contains(_))

          logger.debug(s"Trying next set of peers $peers")
          peers.foreach(tryPeer)
        }
      }
    }

  def start: Future[Unit] = {
    logger.debug(s"Starting PeerFinder")

    (getPeersFromParam ++ getPeersFromConfig).distinct.foreach(tryPeer)

    if (nodeAppConfig.enablePeerDiscovery) {
      for {
        peersFromDb <- getPeersFromDb
      } yield {
        _peersToTry.addAll(peersFromDb)
        _peersToTry.addAll(getPeersFromResources)
        peerConnectionScheduler //start scheduler
        ()
      }
    } else {
      logger.debug("Peer discovery disabled")
      Future.unit
    }
  }

  def stop: Future[Unit] = {
    //stop scheduler
    peerConnectionScheduler.cancel()
    //delete try queue
    _peersToTry.clear()

    _peerData.foreach(_._2.client.close())

    AsyncUtil.retryUntilSatisfied(_peerData.isEmpty,
                                  interval = 1.seconds,
                                  maxTries = 10)
  }

  /** creates and initialises a new test peer */
  def tryPeer(peer: Peer): Unit = {
    _peerData.put(peer, PeerData(peer, node))
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

  def addToTry(peers: Peer*): Unit = {
    _peersToTry.addAll(peers)
  }
}
