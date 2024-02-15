package org.bitcoins.node

import org.bitcoins.core.api.node.{Peer, PeerWithServices}
import org.bitcoins.core.p2p.{CompactFilterMessage, ServiceIdentifier}
import org.bitcoins.node.NodeState.DoneSyncing
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.networking.peer.{PeerConnection, PeerMessageSender}

import scala.util.Random

sealed abstract class NodeState

/** Means our node has been started and is running */
sealed trait NodeRunningState extends NodeState {
  def peerDataMap: Map[PeerWithServices, PersistentPeerData]

  def peersWithServices: Set[PeerWithServices] = peerDataMap.map(_._1).toSet

  /** All peers the node is currently connected to */
  def peers: Set[Peer] = peersWithServices.map(_.peer)

  def waitingForDisconnection: Set[Peer]
  def isSyncing: Boolean

  def peerFinder: PeerFinder

  def getPeerConnection(peer: Peer): Option[PeerConnection] = {
    peerDataMap.find(_._1.peer == peer).map(_._2.peerConnection) match {
      case Some(peerConnection) => Some(peerConnection)
      case None                 => None
    }
  }

  def getPeerMsgSender(peer: Peer)(implicit
      nodeAppConfig: NodeAppConfig): Option[PeerMessageSender] = {
    val randomPeerOpt = getPeerConnection(peer)
    randomPeerOpt.map(PeerMessageSender(_))
  }

  def replacePeers(
      peerDataMap: Map[
        PeerWithServices,
        PersistentPeerData]): NodeRunningState = {
    this match {
      case h: NodeState.HeaderSync =>
        h.copy(peerDataMap = peerDataMap)
      case fh: NodeState.FilterHeaderSync =>
        fh.copy(peerDataMap = peerDataMap)
      case fs: NodeState.FilterSync =>
        fs.copy(peerDataMap = peerDataMap)
      case d: NodeState.DoneSyncing =>
        d.copy(peerDataMap = peerDataMap)
      case rm: NodeState.RemovePeers =>
        rm.copy(peerDataMap = peerDataMap)
      case m: NodeState.MisbehavingPeer =>
        m.copy(peerDataMap = peerDataMap)
      case s: NodeState.NodeShuttingDown =>
        s.copy(peerDataMap = peerDataMap)
    }
  }

  def replaceWaitingForDisconnection(
      newWaitingForDisconnection: Set[Peer]): NodeRunningState = {
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
    }
  }

  def randomPeer(
      excludePeers: Set[Peer],
      services: ServiceIdentifier): Option[Peer] = {
    val filteredPeers =
      peersWithServices
        .filterNot(p => excludePeers.exists(_ == p.peer))
        //don't give peer a peer that we are waiting to disconnect
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
      services: ServiceIdentifier)(implicit
      nodeAppConfig: NodeAppConfig): Option[PeerMessageSender] = {
    randomPeer(excludePeers, services).flatMap { p =>
      getPeerMsgSender(p)
    }
  }

  def isConnected(peer: Peer): Boolean = {
    peerDataMap.filter(_._1.peer == peer).nonEmpty || peerFinder.hasPeer(peer)
  }

  def isDisconnected(peer: Peer): Boolean = !isConnected(peer)

  def toDoneSyncing: DoneSyncing = {
    DoneSyncing(peerDataMap, waitingForDisconnection, peerFinder)
  }
}

/** State to indicate that we are syncing the blockchain */
sealed abstract class SyncNodeState extends NodeRunningState {
  require(
    peers.exists(_ == syncPeer),
    s"syncPeer must be a member of peers, syncPeer=$syncPeer peers=$peers")
  override def isSyncing: Boolean = true

  def syncPeer: Peer

  def replaceSyncPeer(newSyncPeer: Peer): SyncNodeState = {
    this match {
      case h: NodeState.HeaderSync        => h.copy(syncPeer = newSyncPeer)
      case fh: NodeState.FilterHeaderSync => fh.copy(syncPeer = newSyncPeer)
      case fs: NodeState.FilterSync       => fs.copy(syncPeer = newSyncPeer)
    }
  }
}

object NodeState {

  case class HeaderSync(
      syncPeer: Peer,
      peerDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder)
      extends SyncNodeState

  case class FilterHeaderSync(
      syncPeer: Peer,
      peerDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder)
      extends SyncNodeState

  case class FilterSync(
      syncPeer: Peer,
      peerDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      filterBatchCache: Set[CompactFilterMessage],
      peerFinder: PeerFinder)
      extends SyncNodeState {

    override def toString: String = {
      s"FilterSync(syncPeer=$syncPeer,peers=$peers,waitingForDisconnection=$waitingForDisconnection,filterBatchCache.size=${filterBatchCache.size})"
    }
  }

  case class MisbehavingPeer(
      badPeer: Peer,
      peerDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder)
      extends NodeRunningState {
    if (peers.nonEmpty) {
      //needed for the case where the last peer we are connected to is the bad peer
      require(
        peers.exists(_ == badPeer),
        s"MisbehavingPeer must be in peers, badPeer=$badPeer peers=$peers")
    }

    override val isSyncing: Boolean = false
  }

  case class RemovePeers(
      peersToRemove: Vector[Peer],
      peerDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      isSyncing: Boolean,
      peerFinder: PeerFinder)
      extends NodeRunningState {
    require(
      peersToRemove.forall(rm => peers.exists(_ == rm)),
      s"peersToRemove must be subset of peers, peersToRemove=$peersToRemove peers=$peers")
  }

  /** State to indicate we are not currently syncing with a peer */
  case class DoneSyncing(
      peerDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder)
      extends NodeRunningState {
    override val isSyncing: Boolean = false

    /** selects a random peer and returns us a header sync state */
    def toHeaderSync: HeaderSync = {
      val syncPeerOpt =
        randomPeer(Set.empty, ServiceIdentifier.NODE_COMPACT_FILTERS)
      syncPeerOpt match {
        case Some(p) => toHeaderSync(p)
        case None =>
          sys.error(
            s"Could not find a peer to transition from DoneSyncing -> HeaderSync")
      }
    }

    def toHeaderSync(syncPeer: Peer): HeaderSync = {
      HeaderSync(syncPeer, peerDataMap, waitingForDisconnection, peerFinder)
    }
  }

  /** means our node is in the process of shutting down */
  case class NodeShuttingDown(
      peerDataMap: Map[PeerWithServices, PersistentPeerData],
      waitingForDisconnection: Set[Peer],
      peerFinder: PeerFinder)
      extends NodeRunningState {
    override val isSyncing: Boolean = false
  }

}
