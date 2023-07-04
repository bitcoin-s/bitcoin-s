package org.bitcoins.node.networking.peer

import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.NodeState.{
  FilterHeaderSync,
  FilterSync,
  HeaderSync,
  ValidatingHeaders
}

sealed abstract class NodeState {
  def isSyncing: Boolean

}

/** State to indicate that we are syncing the blockchain */
sealed abstract class SyncNodeState extends NodeState {
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

  case class HeaderSync(syncPeer: Peer) extends SyncNodeState

  case class FilterHeaderSync(syncPeer: Peer) extends SyncNodeState

  case class FilterSync(syncPeer: Peer) extends SyncNodeState

  case class ValidatingHeaders(
      syncPeer: Peer,
      inSyncWith: Set[Peer],
      failedCheck: Set[Peer],
      verifyingWith: Set[Peer]
  ) extends SyncNodeState {
    def validated: Boolean = inSyncWith ++ failedCheck == verifyingWith
  }

  case class MisbehavingPeer(badPeer: Peer) extends NodeState {
    override val isSyncing: Boolean = false
  }

  case class RemovePeers(peers: Vector[Peer], isSyncing: Boolean)
      extends NodeState

  /** State to indicate we are not currently syncing with a peer */
  case object DoneSyncing extends NodeState {
    override val isSyncing: Boolean = false
  }
}
