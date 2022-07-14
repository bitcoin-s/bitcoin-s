package org.bitcoins.node

import akka.actor.{ActorRef, ActorSystem, Props}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.core.api.node.NodeType
import org.bitcoins.core.p2p.{
  AddrV2Message,
  ExpectsResponse,
  ServiceIdentifier,
  VersionMessage
}
import org.bitcoins.core.util.{NetworkUtil, StartStopAsync}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{Peer, PeerDAO, PeerDb}
import org.bitcoins.node.networking.peer.PeerMessageSender
import org.bitcoins.node.networking.{P2PClient, P2PClientSupervisor}
import org.bitcoins.node.util.BitcoinSNodeUtil
import scodec.bits.ByteVector

import java.net.InetAddress
import java.time.Duration
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

case class PeerManager(
    paramPeers: Vector[Peer] = Vector.empty,
    node: NeutrinoNode)(implicit
    ec: ExecutionContext,
    system: ActorSystem,
    nodeAppConfig: NodeAppConfig)
    extends StartStopAsync[PeerManager]
    with P2PLogger {

  private val _peerData: mutable.Map[Peer, PeerData] = mutable.Map.empty

  /** holds peers removed from peerData whose client actors are not stopped yet. Used for runtime sanity checks. */
  private val _waitingForDeletion: mutable.Set[Peer] = mutable.Set.empty
  def waitingForDeletion: Set[Peer] = _waitingForDeletion.toSet

  val supervisor: ActorRef =
    system.actorOf(Props[P2PClientSupervisor](),
                   name =
                     BitcoinSNodeUtil.createActorName("P2PClientSupervisor"))

  val finder: PeerFinder =
    PeerFinder(paramPeers, node, skipPeers = () => peers, supervisor)

  def connectedPeerCount: Int = _peerData.size

  def addPeer(peer: Peer): Future[Unit] = {
    require(finder.hasPeer(peer), s"Unknown $peer marked as usable")
    val curPeerData = finder.popFromCache(peer).get
    _peerData.put(peer, curPeerData)
    val hasCf =
      if (curPeerData.serviceIdentifier.nodeCompactFilters) "with filters"
      else ""
    logger.info(
      s"Connected to peer $peer $hasCf. Connected peer count $connectedPeerCount")
    Future.unit
  }

  def peers: Vector[Peer] = _peerData.keys.toVector

  def peerMsgSenders: Vector[PeerMessageSender] =
    _peerData.values
      .map(_.peerMessageSender)
      .toVector

  def clients: Vector[P2PClient] = _peerData.values.map(_.client).toVector

  def randomPeerWithService(services: ServiceIdentifier): Future[Peer] = {
    //wait when requested
    val waitF =
      awaitPeerWithService(services,
                           timeout = nodeAppConfig.peerDiscoveryTimeout)

    waitF.map { _ =>
      val filteredPeers =
        peerData
          .filter(p => p._2.serviceIdentifier.hasServicesOf(services))
          .keys
          .toVector
      assert(filteredPeers.nonEmpty)
      val (good, failedRecently) =
        filteredPeers.partition(p => !peerData(p).hasFailedRecently)

      if (good.nonEmpty) good(Random.nextInt(good.length))
      else
        failedRecently(Random.nextInt(failedRecently.length))
    }
  }

  def randomPeerMsgSenderWithService(
      services: ServiceIdentifier): Future[PeerMessageSender] = {
    val randomPeerF = randomPeerWithService(services)
    randomPeerF.map(peer => peerData(peer).peerMessageSender)
  }

  def createInDb(
      peer: Peer,
      serviceIdentifier: ServiceIdentifier): Future[PeerDb] = {
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
      .upsertPeer(ByteVector(addrBytes),
                  peer.socket.getPort,
                  networkByte,
                  serviceIdentifier)
  }

  def awaitPeerWithService(
      services: ServiceIdentifier,
      timeout: Duration): Future[Unit] = {
    logger.debug(s"Waiting for peer connection. ${_peerData.keys}")

    val ret = AsyncUtil
      .retryUntilSatisfied(
        {
          _peerData.exists(x => x._2.serviceIdentifier.hasServicesOf(services))
        },
        interval = 1.seconds,
        maxTries = timeout.getSeconds.toInt)
      .recover {
        case _: AsyncUtil.RpcRetryException =>
          throw new RuntimeException(
            s"No supported peers found! Requested: ${services}")
        case unknown: Throwable => throw unknown
      }

    ret
  }

  def replacePeer(replacePeer: Peer, withPeer: Peer): Future[Unit] = {
    logger.debug(s"Replacing $replacePeer with $withPeer")
    assert(!peerData(replacePeer).serviceIdentifier.nodeCompactFilters,
           s"$replacePeer has cf")
    removePeer(replacePeer)
    addPeer(withPeer)
  }

  def removePeer(peer: Peer): Unit = {
    logger.debug(s"Removing persistent peer $peer")
    val client = peerData(peer).client
    _peerData.remove(peer)
    //so we need to remove if from the map for connected peers so no more request could be sent to it but we before
    //the actor is stopped we don't delete it to ensure that no such case where peers is deleted but actor not stopped
    //leading to a memory leak may happen
    _waitingForDeletion.add(peer)
    //now send request to stop actor which will be completed some time in future
    client.close()
  }

  def isReconnection(peer: Peer): Boolean = {
    peerData.contains(peer)
  }

  override def start(): Future[PeerManager] = {
    logger.debug(s"Starting PeerManager")
    finder.start().map { _ =>
      logger.info("Done starting PeerManager")
      this
    }
  }

  def peerData: Map[Peer, PeerData] = _peerData.toMap

  override def stop(): Future[PeerManager] = {
    logger.info(s"Stopping PeerManager")
    val beganAt = System.currentTimeMillis()

    val finderStopF = finder.stop()

    peers.foreach(removePeer)

    val managerStopF = AsyncUtil.retryUntilSatisfied(
      _peerData.isEmpty && waitingForDeletion.isEmpty,
      interval = 1.seconds,
      maxTries = 10)

    for {
      _ <- finderStopF
      _ <- managerStopF
    } yield {
      logger.info(
        s"Stopped PeerManager. Took ${System.currentTimeMillis() - beganAt} ms ")
      this
    }
  }

  def isConnected(peer: Peer): Future[Boolean] = {
    if (peerData.contains(peer))
      peerData(peer).peerMessageSender.isConnected()
    else Future.successful(false)
  }

  def isInitialized(peer: Peer): Future[Boolean] = {
    if (peerData.contains(peer))
      peerData(peer).peerMessageSender.isInitialized()
    else Future.successful(false)
  }

  def onInitializationTimeout(peer: Peer): Unit = {
    assert(!finder.hasPeer(peer) || !peerData.contains(peer),
           s"$peer cannot be both a test and a persistent peer")

    if (finder.hasPeer(peer)) {
      //one of the peers that we tried, failed to init within time, disconnect
      finder.getData(peer).client.close()
    } else if (peerData.contains(peer)) {
      //this is one of our persistent peers which must have been initialized earlier, this can happen in case of
      //a reconnection attempt, meaning it got connected but failed to initialize, disconnect
      peerData(peer).client.close()
    } else {
      //this should never happen
      logger.warn(s"onInitializationTimeout called for unknown $peer")
    }
  }

  def onInitialization(peer: Peer): Future[Unit] = {
    assert(!finder.hasPeer(peer) || !peerData.contains(peer),
           s"$peer cannot be both a test and a persistent peer")

    //this assumes neutrino and checks for compact filter support so should not be called for anything else
    assert(nodeAppConfig.nodeType == NodeType.NeutrinoNode,
           s"Node cannot be ${nodeAppConfig.nodeType.shortName}")

    if (finder.hasPeer(peer)) {
      //one of the peers we tries got initialized successfully
      val hasCf = finder.getData(peer).serviceIdentifier.nodeCompactFilters

      logger.debug(s"Initialized peer $peer with $hasCf")

      def sendAddrReq: Future[Unit] =
        finder.getData(peer).peerMessageSender.sendGetAddrMessage()

      def managePeerF(): Future[Unit] = {
        //if we have slots remaining, connect
        if (connectedPeerCount < nodeAppConfig.maxConnectedPeers) {
          addPeer(peer)
        } else {
          lazy val notCf = peerData
            .filter(p => !p._2.serviceIdentifier.nodeCompactFilters)
            .keys

          //try to drop another non compact filter connection for this
          if (hasCf && notCf.nonEmpty)
            replacePeer(replacePeer = notCf.head, withPeer = peer)
          else {
            //no use for this apart from writing in db
            //we do want to give it enough time to send addr messages
            AsyncUtil
              .nonBlockingSleep(duration = 10.seconds)
              .map { _ =>
                //could have already been deleted in case of connection issues
                if (finder.hasPeer(peer))
                  finder.getData(peer).client.close()
              }
          }
        }
      }

      for {
        _ <- sendAddrReq
        _ <- createInDb(peer, finder.getData(peer).serviceIdentifier)
        _ <- managePeerF()
      } yield ()

    } else if (peerData.contains(peer)) {
      //one of the persistent peers initialized again, this can happen in case of a reconnection attempt
      //which succeeded which is all good, do nothing
      Future.unit
    } else {
      logger.warn(s"onInitialization called for unknown $peer")
      Future.unit
    }
  }

  def onP2PClientStopped(peer: Peer): Future[Unit] = {
    assert(!finder.hasPeer(peer) || !peerData.contains(peer),
           s"$peer cannot be both a test and a persistent peer")

    logger.debug(s"Client stopped for $peer")

    if (finder.hasPeer(peer)) {
      //client actor for one of the test peers stopped, can remove it from map now
      finder.removePeer(peer)
      Future.unit
    } else if (peerData.contains(peer)) {
      //actor stopped for one of the persistent peers, can happen in case a reconnection attempt failed due to
      //reconnection tries exceeding the max limit in which the client was stopped to disconnect from it, remove it
      _peerData.remove(peer)
      val syncPeer = node.getDataMessageHandler.syncPeer
      if (syncPeer.isDefined && syncPeer.get == peer)
        syncFromNewPeer()
      else Future.unit
    } else if (waitingForDeletion.contains(peer)) {
      //a peer we wanted to disconnect has remove has stopped the client actor, finally mark this as deleted
      _waitingForDeletion.remove(peer)
      Future.unit
    } else {
      logger.warn(s"onP2PClientStopped called for unknown $peer")
      Future.unit
    }
  }

  def onVersionMessage(peer: Peer, versionMsg: VersionMessage): Unit = {
    assert(!finder.hasPeer(peer) || !peerData.contains(peer),
           s"$peer cannot be both a test and a persistent peer")

    if (finder.hasPeer(peer)) {
      finder.getData(peer).setServiceIdentifier(versionMsg.services)
    } else if (peerData.contains(peer)) {
      assert(
        peerData(peer).serviceIdentifier.bytes == versionMsg.services.bytes)
    } else {
      logger.warn(s"onVersionMessage called for unknown $peer")
    }
  }

  def onQueryTimeout(payload: ExpectsResponse, peer: Peer): Future[Unit] = {
    logger.debug(s"Query timeout out for $peer")
    payload match {
      case _ => //if any times out, try a new peer
        peerData(peer).updateLastFailureTime()
        val syncPeer = node.getDataMessageHandler.syncPeer
        if (syncPeer.isDefined && syncPeer.get == peer)
          syncFromNewPeer()
        else Future.unit
    }
  }

  def onReconnect(peer: Peer): Future[Unit] = {
    logger.debug(s"Reconnected with $peer")
    Future.unit
  }

  def syncFromNewPeer(): Future[Unit] = {
    logger.debug(s"Trying to sync from new peer")
    val newNode =
      node.updateDataMessageHandler(node.getDataMessageHandler.reset)
    newNode.sync()
  }
}
