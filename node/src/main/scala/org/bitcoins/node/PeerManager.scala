package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.core.p2p.{AddrV2Message, ServiceIdentifier}
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageSender
import scodec.bits.ByteVector

import java.net.{InetAddress, UnknownHostException}
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Random

case class PeerManager(node: Node, configPeers: Vector[Peer] = Vector.empty)(
    implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig)
    extends P2PLogger {

  private val _peerData: mutable.Map[Peer, PeerData] =
    mutable.Map.empty

  def peerData: Map[Peer, PeerData] = _peerData.toMap

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

  /** Returns peers randomly taken from config, database, hardcoded peers, dns seeds in that order */
  def getPeers: Future[Vector[Peer]] = {
    //currently this would only give the first peer from config
    val peersFromConfig = getPeersFromConfig
    val peersFromDbF = getPeersFromDb
    val peersFromResources = getPeersFromResources
    val maxConnectedPeers = nodeAppConfig.maxConnectedPeers

    val allF = for {
      peersFromDb <- peersFromDbF
    } yield {
      val shuffledPeers = (Random.shuffle(peersFromConfig) ++ Random.shuffle(
        peersFromDb) ++ Random.shuffle(peersFromResources)).distinct

      //getting peers from dns seeds takes a noticeable 5-8 sec so treating this separately
      if (maxConnectedPeers > shuffledPeers.size) {
        shuffledPeers.take(maxConnectedPeers) ++ getPeersFromDnsSeeds.take(
          maxConnectedPeers - shuffledPeers.size)
      } else {
        shuffledPeers.take(maxConnectedPeers)
      }
    }
    allF
  }

  def addPeer(peer: Peer): Unit = {
    if (!_peerData.contains(peer))
      _peerData.put(peer, PeerData(peer, node))
    else logger.debug(s"Peer $peer already added.")
    ()
  }

  def removePeer(peer: Peer): Future[Unit] = {
    if (_peerData.contains(peer)) {
      val connF = peerData(peer).peerMessageSender.isConnected()
      val disconnectF = connF.map { conn =>
        if (conn) peerData(peer).peerMessageSender.disconnect()
        else Future.unit
      }
      for {
        _ <- disconnectF
        _ = _peerData.remove(peer)
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
  def peerDataOf(peer: Peer): PeerData = peerData(peer)
}
