package org.bitcoins.node.networking.peer

import org.bitcoins.node.models.Peer

sealed abstract class DataMessageHandlerState {
  def isSyncing: Boolean

}

/** State to indicate that we are syncing the blockchain */
sealed abstract class SyncDataMessageHandlerState
    extends DataMessageHandlerState {
  override def isSyncing: Boolean = true

  def syncPeer: Peer
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

  /** State to indicate we are not currently syncing with a peer */
  case object DoneSyncing extends DataMessageHandlerState {
    override val isSyncing: Boolean = false
  }
}
