package org.bitcoins.node

import org.bitcoins.core.api.node.{Peer, PeerWithServices}
import org.bitcoins.core.p2p.{
  CompactFilterMessage,
  NetworkMessage,
  ServiceIdentifier
}
import org.bitcoins.node.NodeState.{
  DoneSyncing,
  FilterHeaderSync,
  FilterSync,
  MisbehavingPeer,
  NodeShuttingDown,
  RemovePeers
}
import org.bitcoins.node.networking.peer.{PeerConnection, PeerMessageSender}

import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

sealed abstract class NodeState

/** Means our node has been started and is running */
sealed trait NodeRunningState extends NodeState {
  def peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData]

  def peerDataMap: Map[Peer, PersistentPeerData] = peerWithServicesDataMap.map {
    case (k, v) => (k.peer, v)
  }

  def peersWithServices: Set[PeerWithServices] =
    peerWithServicesDataMap.map(_._1).toSet

  /** All peers the node is currently connected to */
  def peers: Set[Peer] = peersWithServices.map(_.peer)

  def waitingForDisconnection: Set[Peer]
  def isSyncing: Boolean

  def peerFinder: PeerFinder

  def connectedPeerCount: Int = peers.size

  def getPeerData(peer: Peer): Option[PersistentPeerData] = {
    peerDataMap.find(_._1 == peer).map(_._2)
  }

  def getPeerConnection(peer: Peer): Option[PeerConnection] = {
    peerDataMap.find(_._1 == peer).map(_._2.peerConnection) match {
      case Some(peerConnection) => Some(peerConnection)
      case None                 => None
    }
  }

  def getPeerMsgSender(peer: Peer): Option[PeerMessageSender] = {
    val randomPeerOpt = getPeerConnection(peer)
    randomPeerOpt.map(PeerMessageSender(_))
  }

  def getPeerServices(peer: Peer): Option[ServiceIdentifier] = {
    peersWithServices.find(_.peer == peer).map(_.services)
  }

  def replacePeers(
      peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData]
  ): NodeRunningState = {
    if (peerWithServicesDataMap.isEmpty) {
      NodeState.NoPeers(waitingForDisconnection, peerFinder, Vector.empty)
    } else {
      this match {
        case h: NodeState.HeaderSync =>
          h.copy(peerWithServicesDataMap = peerWithServicesDataMap)
        case fh: NodeState.FilterHeaderSync =>
          fh.copy(peerWithServicesDataMap = peerWithServicesDataMap)
        case fs: NodeState.FilterSync =>
          fs.copy(peerWithServicesDataMap = peerWithServicesDataMap)
        case d: NodeState.DoneSyncing =>
          d.copy(peerWithServicesDataMap = peerWithServicesDataMap)
        case rm: NodeState.RemovePeers =>
          rm.copy(peerWithServicesDataMap = peerWithServicesDataMap)
        case m: NodeState.MisbehavingPeer =>
          m.copy(peerWithServicesDataMap = peerWithServicesDataMap)
        case s: NodeState.NodeShuttingDown =>
          s.copy(peerWithServicesDataMap = peerWithServicesDataMap)
        case n: NodeState.NoPeers =>
          DoneSyncing(peerWithServicesDataMap,
                      n.waitingForDisconnection,
                      n.peerFinder)
      }
    }

  }

  def addPeer(peer: Peer): NodeRunningState = {
    val peerDataOpt = peerFinder.popFromCache(peer)
    peerDataOpt match {
      case None =>
        // do we just want to ignore the attempt if
        // the peer does not exist??
        this
      case Some(peerData) =>
        val persistentPeerData = peerData match {
          case p: PersistentPeerData       => p
          case a: AttemptToConnectPeerData => a.toPersistentPeerData
        }
        val peerWithSvcs = persistentPeerData.peerWithServicesOpt.get
        val newPdm =
          peerWithServicesDataMap.+((peerWithSvcs, persistentPeerData))
        val newState = replacePeers(newPdm)
        newState
    }

  }

  /** Removes the peer from our [[peerDataMap]] */
  def removePeer(peer: Peer): NodeRunningState = {
    val filtered = peerWithServicesDataMap.filterNot(_._1.peer == peer)
    this match {
      case sync: SyncNodeState =>
        if (sync.syncPeer == peer) {
          sync.replaceSyncPeer match {
            case Some(newSyncNodeState) =>
              newSyncNodeState.replacePeers(filtered)
            case None =>
              sync.toDoneSyncing.replacePeers(filtered)
          }
        } else {
          sync.replacePeers(filtered)
        }
      case x @ (_: DoneSyncing | _: MisbehavingPeer | _: RemovePeers |
          _: NodeShuttingDown) =>
        x.replacePeers(filtered)
      case n: NodeState.NoPeers =>
        sys.error(s"Cannot remove peer=$peer when we have no peers! $n")
    }
  }

  def replaceWaitingForDisconnection(
      newWaitingForDisconnection: Set[Peer]
  ): NodeRunningState = {
    this match {
      case h: NodeState.HeaderSync =>
        h.copy(waitingForDisconnection = newWaitingForDisconnection)
      case fh: NodeState.FilterHeaderSync =>
        fh.copy(waitingForDisconnection = newWaitingForDisconnection)
      case fs: NodeState.FilterSync =>
        fs.copy(waitingForDisconnection = newWaitingForDisconnection)
      case d: NodeState.DoneSyncing =>
        d.copy(waitingForDisconnection = newWaitingForDisconnection)
      case rm: NodeState.RemovePeers =>
        rm.copy(waitingForDisconnection = newWaitingForDisconnection)
      case m: NodeState.MisbehavingPeer =>
        m.copy(waitingForDisconnection = newWaitingForDisconnection)
      case s: NodeState.NodeShuttingDown =>
        s.copy(waitingForDisconnection = newWaitingForDisconnection)
      case n: NodeState.NoPeers =>
        n.copy(waitingForDisconnection = newWaitingForDisconnection)
    }
  }

  def randomPeer(
      excludePeers: Set[Peer],
      services: ServiceIdentifier
  ): Option[Peer] = {
    val filteredPeers =
      peersWithServices
        .filterNot(p => excludePeers.exists(_ == p.peer))
        // don't give peer a peer that we are waiting to disconnect
        .filterNot(p => waitingForDisconnection.exists(_ == p.peer))
        .filter(p => p.services.hasServicesOf(services))
        .toVector

    val peerOpt = if (filteredPeers.nonEmpty) {
      Some(filteredPeers(Random.nextInt(filteredPeers.length)))
    } else {
      None
    }
    peerOpt.map(_.peer)
  }

  def randomPeerMessageSender(
      excludePeers: Set[Peer],
      services: ServiceIdentifier
  ): Option[PeerMessageSender] = {
    randomPeer(excludePeers, services).flatMap { p =>
      getPeerMsgSender(p)
    }
  }

  def isConnected(peer: Peer): Boolean = {
    peerDataMap.filter(_._1 == peer).nonEmpty || peerFinder.hasPeer(peer)
  }

  def isDisconnected(peer: Peer): Boolean = !isConnected(peer)

  override def toString: String = {
    s"${getClass.getSimpleName}(peers=${peers},waitingForDisconnection=${waitingForDisconnection})"
  }
}

/** State to indicate that we are syncing the blockchain */
sealed abstract class SyncNodeState extends NodeRunningState {
  require(
    peers.exists(_ == syncPeer),
    s"syncPeer must be a member of peers, syncPeer=$syncPeer peers=$peers"
  )
  override def isSyncing: Boolean = true

  def syncPeer: Peer

  /** Services of our [[syncPeer]] */
  def services: ServiceIdentifier = getPeerServices(syncPeer).get

  def syncPeerMessageSender =
    getPeerMsgSender(syncPeer).get

  def replaceSyncPeer(newSyncPeer: Peer): SyncNodeState = {
    this match {
      case h: NodeState.HeaderSync        => h.copy(syncPeer = newSyncPeer)
      case fh: NodeState.FilterHeaderSync => fh.copy(syncPeer = newSyncPeer)
      case fs: NodeState.FilterSync       => fs.copy(syncPeer = newSyncPeer)
    }
  }

  /** Replaces the current sync peer with a new sync peer, returns None if there
    * is not a new peer available
    */
  def replaceSyncPeer: Option[SyncNodeState] = {
    randomPeer(
      excludePeers = Set(syncPeer),
      ServiceIdentifier.NODE_COMPACT_FILTERS
    ).map { p =>
      replaceSyncPeer(p)
    }
  }

  def toFilterHeaderSync: FilterHeaderSync = {
    FilterHeaderSync(
      syncPeer,
      peerWithServicesDataMap = peerWithServicesDataMap,
      waitingForDisconnection = waitingForDisconnection,
      peerFinder = peerFinder,
      sentQuery = Instant.now()
    )
  }

  def toFilterSync: FilterSync = {
    FilterSync(
      syncPeer = syncPeer,
      peerWithServicesDataMap = peerWithServicesDataMap,
      waitingForDisconnection = waitingForDisconnection,
      filterBatchCache = Set.empty,
      peerFinder = peerFinder,
      sentQuery = Instant.now
    )
  }

  def toDoneSyncing: DoneSyncing = {
    DoneSyncing(peerWithServicesDataMap, waitingForDisconnection, peerFinder)
  }

  /** The time when we sent the last query */
  def sentQuery: Instant

  def isQueryTimedOut(duration: FiniteDuration): Boolean = {
    val timeout = Instant.now().minus(duration.toMillis, ChronoUnit.MILLIS)
    sentQuery.isBefore(timeout)
  }

  override def toString: String = {
    s"${getClass.getSimpleName}(syncPeer=$syncPeer,peers=${peers},waitingForDisconnection=${waitingForDisconnection})"
  }
}

/** Either we are syncing [[NodeState.FilterHeaderSync]] or
  * [[NodeState.FilterSync]]
  */
sealed trait FilterOrFilterHeaderSync extends SyncNodeState

object NodeState {

  case class HeaderSync(
      syncPeer: Peer,
      peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder,
      sentQuery: Instant
  ) extends SyncNodeState

  case class FilterHeaderSync(
      syncPeer: Peer,
      peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder,
      sentQuery: Instant
  ) extends FilterOrFilterHeaderSync

  case class FilterSync(
      syncPeer: Peer,
      peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      filterBatchCache: Set[CompactFilterMessage],
      peerFinder: PeerFinder,
      sentQuery: Instant
  ) extends FilterOrFilterHeaderSync {

    override def toString: String = {
      s"FilterSync(syncPeer=$syncPeer,peers=$peers,waitingForDisconnection=$waitingForDisconnection,filterBatchCache.size=${filterBatchCache.size})"
    }
  }

  case class MisbehavingPeer(
      badPeer: Peer,
      peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder
  ) extends NodeRunningState {
    if (peers.nonEmpty) {
      // needed for the case where the last peer we are connected to is the bad peer
      require(
        peers.exists(_ == badPeer),
        s"MisbehavingPeer must be in peers, badPeer=$badPeer peers=$peers"
      )
    }

    override val isSyncing: Boolean = false
  }

  case class RemovePeers(
      peersToRemove: Vector[Peer],
      peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      isSyncing: Boolean,
      peerFinder: PeerFinder
  ) extends NodeRunningState {
    require(
      peersToRemove.forall(rm => peers.exists(_ == rm)),
      s"peersToRemove must be subset of peers, peersToRemove=$peersToRemove peers=$peers"
    )

    /** Means we have no good peers are removing these peers */
    def isEmpty: Boolean = {
      peerWithServicesDataMap.keys.toSet == peersToRemove.toSet
    }
  }

  /** State to indicate we are not currently syncing with a peer */
  case class DoneSyncing(
      peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder
  ) extends NodeRunningState {
    require(
      peerWithServicesDataMap.nonEmpty,
      s"Cannot have 0 peers, use NoPeers to represent state where we have 0 peers")
    override val isSyncing: Boolean = false

    /** Selects a random peer and returns us a header sync state returns None if
      * we don't have a peer ot sync with
      */
    def toHeaderSync: Option[HeaderSync] = {
      val syncPeerOpt =
        randomPeer(Set.empty, ServiceIdentifier.NODE_COMPACT_FILTERS)
      syncPeerOpt.map(toHeaderSync)
    }

    def toHeaderSync(syncPeer: Peer): HeaderSync = {
      HeaderSync(
        syncPeer = syncPeer,
        peerWithServicesDataMap = peerWithServicesDataMap,
        waitingForDisconnection = waitingForDisconnection,
        peerFinder = peerFinder,
        sentQuery = Instant.now()
      )
    }

    def toFilterHeaderSync: Option[FilterHeaderSync] = {
      val syncPeerOpt =
        randomPeer(Set.empty, ServiceIdentifier.NODE_COMPACT_FILTERS)
      syncPeerOpt.map(toFilterHeaderSync)
    }

    def toFilterHeaderSync(syncPeer: Peer): FilterHeaderSync = {
      FilterHeaderSync(
        syncPeer = syncPeer,
        peerWithServicesDataMap = peerWithServicesDataMap,
        waitingForDisconnection = waitingForDisconnection,
        peerFinder = peerFinder,
        sentQuery = Instant.now()
      )
    }
  }

  /** means our node is in the process of shutting down */
  case class NodeShuttingDown(
      peerWithServicesDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder
  ) extends NodeRunningState {
    override val isSyncing: Boolean = false
  }

  case class NoPeers(
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder,
      cachedOutboundMessages: Vector[NetworkMessage])
      extends NodeRunningState {
    override val isSyncing: Boolean = false
    override val peerWithServicesDataMap: Map[PeerWithServices, Nothing] =
      Map.empty

    def toDoneSyncing(
        map: Map[PeerWithServices, PersistentPeerData]): DoneSyncing = {
      // discards cached messages? Probably need to send them before returning?
      DoneSyncing(peerWithServicesDataMap = map,
                  waitingForDisconnection = waitingForDisconnection,
                  peerFinder = peerFinder)
    }
  }

}
