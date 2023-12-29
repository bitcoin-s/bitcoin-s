package org.bitcoins.core.api.node

import org.bitcoins.core.p2p.{CompactFilterMessage, ServiceIdentifier}

import scala.util.Random

sealed abstract class NodeState {
  def isSyncing: Boolean

  def peersWithServices: Set[PeerWithServices]

  /** All peers the node is currently connected to */
  def peers: Set[Peer] = peersWithServices.map(_.peer)

  def waitingForDisconnection: Set[Peer]

  def replacePeers(peerWithServices: Set[PeerWithServices]): NodeState =
    this match {
      case h: NodeState.HeaderSync =>
        h.copy(peersWithServices = peerWithServices)
      case fh: NodeState.FilterHeaderSync =>
        fh.copy(peersWithServices = peerWithServices)
      case fs: NodeState.FilterSync =>
        fs.copy(peersWithServices = peerWithServices)
      case d: NodeState.DoneSyncing =>
        d.copy(peersWithServices = peerWithServices)
      case rm: NodeState.RemovePeers =>
        rm.copy(peersWithServices = peerWithServices)
      case m: NodeState.MisbehavingPeer =>
        m.copy(peersWithServices = peerWithServices)
    }

  def replaceWaitingForDisconnection(
      newWaitingForDisconnection: Set[Peer]): NodeState = {
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
}

/** State to indicate that we are syncing the blockchain */
sealed abstract class SyncNodeState extends NodeState {
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
      peersWithServices: Set[PeerWithServices],
      waitingForDisconnection: Set[Peer])
      extends SyncNodeState

  case class FilterHeaderSync(
      syncPeer: Peer,
      peersWithServices: Set[PeerWithServices],
      waitingForDisconnection: Set[Peer])
      extends SyncNodeState

  case class FilterSync(
      syncPeer: Peer,
      peersWithServices: Set[PeerWithServices],
      waitingForDisconnection: Set[Peer],
      filterBatchCache: Set[CompactFilterMessage])
      extends SyncNodeState {

    override def toString: String = {
      s"FilterSync(syncPeer=$syncPeer,peers=$peers,waitingForDisconnection=$waitingForDisconnection,filterBatchCache.size=${filterBatchCache.size})"
    }
  }

  case class MisbehavingPeer(
      badPeer: Peer,
      peersWithServices: Set[PeerWithServices],
      waitingForDisconnection: Set[Peer])
      extends NodeState {
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
      peersWithServices: Set[PeerWithServices],
      waitingForDisconnection: Set[Peer],
      isSyncing: Boolean)
      extends NodeState {
    require(
      peersToRemove.forall(rm => peers.exists(_ == rm)),
      s"peersToRemove must be subset of peers, peersToRemove=$peersToRemove peers=$peers")
  }

  /** State to indicate we are not currently syncing with a peer */
  case class DoneSyncing(
      peersWithServices: Set[PeerWithServices],
      waitingForDisconnection: Set[Peer])
      extends NodeState {
    override val isSyncing: Boolean = false
  }
}
