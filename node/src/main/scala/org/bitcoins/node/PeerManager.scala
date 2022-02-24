package org.bitcoins.node

import akka.actor.{ActorSystem, Cancellable}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.p2p.{AddrV2Message, ServiceIdentifier}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageSender
import scodec.bits.ByteVector

import java.net.{InetAddress, UnknownHostException}
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

  private def peerData: Map[Peer, PeerData] = _peerData.toMap

  def testPeerData: Map[Peer, PeerData] = _testPeerData.toMap

  def connectedPeerCount: Int = peerData.size

  //stack to store peers to connect to as part of peer discovery
  val peerDiscoveryStack: mutable.Stack[Peer] = mutable.Stack.empty[Peer]

  val maxPeerSearchCount =
    1000 //number of peers in db at which we stop peer discovery

  val peerConnectionScheduler: Cancellable =
    system.scheduler.scheduleWithFixedDelay(initialDelay = 15.seconds,
                                            delay = 8.seconds) {
      new Runnable() {
        override def run(): Unit = {
          val peersInDbCountF = PeerDAO().count()
          peersInDbCountF.map(cnt =>
            if (cnt > maxPeerSearchCount) peerConnectionScheduler.cancel())

          if (peerDiscoveryStack.size < 8)
            peerDiscoveryStack.pushAll(getPeersFromDnsSeeds)

          if(testPeerData.size < 16) {
            val peers = for {_ <- 0 to 7} yield peerDiscoveryStack.pop()
            peers.foreach(peer => {
              addTestPeer(peer)
            })
          }
        }
      }
    }

  /** moves a peer from [[_testPeerData]] to [[_peerData]]
    * this operation makes the node permanently keep connection with the peer and
    * use it for node operation
    */
  def setPeerForUse(peer: Peer): Unit = {
    require(testPeerData.contains(peer), "Unknown peer marked as usable")
    _peerData.addOne((peer, peerDataOf(peer)))
    _testPeerData.remove(peer)
    ()
    //todo
    //peerData(peer).peerMessageSender.sendGetAddrMessage()
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
  private def getPeersFromConfig: Vector[Peer] = {
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

  /** Returns peers randomly taken from config, database */
  def getPeers: Future[Vector[Peer]] = {
    val peersFromConfig = getPeersFromConfig
    val peersFromDbF = getPeersFromDb

    val allF = for {
      peersFromDb <- peersFromDbF
    } yield {
      val shuffledPeers = (Random.shuffle(peersFromConfig) ++ Random.shuffle(
        peersFromDb)).distinct
      shuffledPeers
    }
    allF
  }

  /** initial setup for peer discovery. Does the following:
    * load peers from resources into discovery stack
    * starts connecting with config and db peers.
    */
  def initSetup: Future[Unit] = {
    val peersF = getPeers
    val peersFromResources = getPeersFromResources
    peerDiscoveryStack.pushAll(peersFromResources)

    peersF.map(_.foreach(addTestPeer))
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
      val connF = testPeerData(peer).peerMessageSender.isConnected()
      val disconnectF = connF.map { conn =>
        if (conn) testPeerData(peer).peerMessageSender.disconnect()
        else Future.unit
      }
      for {
        _ <- disconnectF
        _ = _testPeerData.remove(peer)
      } yield ()
    } else {
      logger.debug(s"Key $peer not found in peerData")
      Future.unit
    }
  }

  def randomPeerMsgSenderWithService(
      f: ServiceIdentifier => Boolean): PeerMessageSender = {
    val filteredPeers =
      peerData.values.filter(p => f(p.serviceIdentifier)).toVector
    if (filteredPeers.isEmpty)
      throw new RuntimeException("No peers supporting compact filters!")
    val randomPeerData = filteredPeers(Random.nextInt(filteredPeers.length))
    randomPeerData.peerMessageSender
  }

  def randomPeerMsgSenderWithCompactFilters: PeerMessageSender = {
    randomPeerMsgSenderWithService(_.nodeCompactFilters)
  }

  def randomPeerMsgSender: PeerMessageSender = {
    peerMsgSenders(Random.nextInt(peerMsgSenders.length))
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
  def peerDataOf(peer: Peer): PeerData = testPeerData(peer)

  def awaitPeerWithService(f: ServiceIdentifier => Boolean): Future[Unit] = {
    logger.info("Waiting for peer connection")
    AsyncUtil
      .retryUntilSatisfied(peerData.exists(x => f(x._2.serviceIdentifier)),
                           interval = 1.seconds,
                           maxTries = 600 //times out in 10 minutes
      )
      .map(_ => logger.info("Connected to peer. Starting sync."))
  }

  def initializePeer(peer: Peer): Future[Unit] = {
    peerDataOf(peer).peerMessageSender.connect()
    val isInitializedF =
      for {
        _ <- AsyncUtil
          .retryUntilSatisfiedF(
            () => peerDataOf(peer).peerMessageSender.isInitialized(),
            maxTries = 10,
            interval = 250.millis)
          .recover { case NonFatal(_) =>
            logger.info(s"Failed to initialize $peer")
            removeTestPeer(peer);
          }
      } yield ()
    isInitializedF
  }
}
