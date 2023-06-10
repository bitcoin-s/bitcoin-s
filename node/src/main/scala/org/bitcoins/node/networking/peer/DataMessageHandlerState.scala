package org.bitcoins.node.networking.peer

import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.peer.DataMessageHandlerState.{
  FilterHeaderSync,
  FilterSync,
  HeaderSync,
  ValidatingHeaders
}

sealed abstract class DataMessageHandlerState {
  def isSyncing: Boolean

}

/** State to indicate that we are syncing the blockchain */
sealed abstract class SyncDataMessageHandlerState
    extends DataMessageHandlerState {
  override def isSyncing: Boolean = true

  def syncPeer: Peer

  def replaceSyncPeer(newSyncPeer: Peer): SyncDataMessageHandlerState = {
    this match {
      case h: HeaderSync         => h.copy(syncPeer = newSyncPeer)
      case fh: FilterHeaderSync  => fh.copy(syncPeer = newSyncPeer)
      case fs: FilterSync        => fs.copy(syncPeer = newSyncPeer)
      case vh: ValidatingHeaders => vh.copy(syncPeer = newSyncPeer)
    }
  }
}

object DataMessageHandlerState {

  case class HeaderSync(syncPeer: Peer) extends SyncDataMessageHandlerState

  case class FilterHeaderSync(syncPeer: Peer)
      extends SyncDataMessageHandlerState

  case class FilterSync(syncPeer: Peer) extends SyncDataMessageHandlerState

  case class ValidatingHeaders(
      syncPeer: Peer,
      inSyncWith: Set[Peer],
      failedCheck: Set[Peer],
      verifyingWith: Set[Peer]
  ) extends SyncDataMessageHandlerState {
    def validated: Boolean = inSyncWith ++ failedCheck == verifyingWith
  }

  case class MisbehavingPeer(badPeer: Peer) extends DataMessageHandlerState {
    override val isSyncing: Boolean = false
  }

  case class RemovePeers(peers: Vector[Peer], isSyncing: Boolean)
      extends DataMessageHandlerState

  /** State to indicate we are not currently syncing with a peer */
  case object DoneSyncing extends DataMessageHandlerState {
    override val isSyncing: Boolean = false
  }
}
