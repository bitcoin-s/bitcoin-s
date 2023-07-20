package org.bitcoins.node.networking.peer

import org.bitcoins.core.api.node.Peer
import org.bitcoins.node.networking.peer.NodeState.{
  DoneSyncing,
  FilterHeaderSync,
  FilterSync,
  HeaderSync,
  MisbehavingPeer,
  RemovePeers,
  ValidatingHeaders
}

sealed abstract class NodeState {
  def isSyncing: Boolean

  /** All peers the node is currently connected to */
  def peers: Set[Peer]

  def replacePeers(newPeers: Set[Peer]): NodeState = this match {
    case h: HeaderSync        => h.copy(peers = newPeers)
    case fh: FilterHeaderSync => fh.copy(peers = newPeers)
    case fs: FilterSync       => fs.copy(peers = newPeers)
    case d: DoneSyncing       => d.copy(peers = newPeers)
    case v: ValidatingHeaders => v.copy(peers = newPeers)
    case rm: RemovePeers      => rm.copy(peers = newPeers)
    case m: MisbehavingPeer   => m.copy(peers = newPeers)
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
      case h: HeaderSync         => h.copy(syncPeer = newSyncPeer)
      case fh: FilterHeaderSync  => fh.copy(syncPeer = newSyncPeer)
      case fs: FilterSync        => fs.copy(syncPeer = newSyncPeer)
      case vh: ValidatingHeaders => vh.copy(syncPeer = newSyncPeer)
    }
  }
}

object NodeState {

  case class HeaderSync(syncPeer: Peer, peers: Set[Peer]) extends SyncNodeState

  case class FilterHeaderSync(syncPeer: Peer, peers: Set[Peer])
      extends SyncNodeState

  case class FilterSync(syncPeer: Peer, peers: Set[Peer]) extends SyncNodeState

  case class ValidatingHeaders(
      syncPeer: Peer,
      inSyncWith: Set[Peer],
      failedCheck: Set[Peer],
      verifyingWith: Set[Peer],
      peers: Set[Peer]
  ) extends SyncNodeState {
    def validated: Boolean = inSyncWith ++ failedCheck == verifyingWith

    override def toString: String = {
      s"ValidatingHeaders(syncPeer=$syncPeer,inSyncWith=$inSyncWith,failedCheck=$failedCheck,verifyingWith=$verifyingWith,peers=$peers)"
    }
  }

  case class MisbehavingPeer(badPeer: Peer, peers: Set[Peer])
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
      peers: Set[Peer],
      isSyncing: Boolean)
      extends NodeState {
    require(
      peersToRemove.forall(rm => peers.exists(_ == rm)),
      s"peersToRemove must be subset of peers, peersToRemove=$peersToRemove peers=$peers")
  }

  /** State to indicate we are not currently syncing with a peer */
  case class DoneSyncing(peers: Set[Peer]) extends NodeState {
    override val isSyncing: Boolean = false
  }
}
