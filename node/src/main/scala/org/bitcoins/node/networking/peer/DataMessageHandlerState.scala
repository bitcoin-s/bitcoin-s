package org.bitcoins.node.networking.peer

import org.bitcoins.node.models.Peer

sealed abstract class DataMessageHandlerState {
  def isSyncing: Boolean

  def syncPeer: Peer
}

sealed abstract class SyncDataMessageHandlerState
    extends DataMessageHandlerState {
  override def isSyncing: Boolean = true
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

  case class DoneSyncing(syncPeer: Peer) extends DataMessageHandlerState {
    override val isSyncing: Boolean = false
  }
}
