package org.bitcoins.core.api.node

sealed abstract class NodeState {
  def isSyncing: Boolean

  /** All peers the node is currently connected to */
  def peers: Set[Peer]

  def waitingForDisconnection: Set[Peer]

  def replacePeers(newPeers: Set[Peer]): NodeState = this match {
    case h: NodeState.HeaderSync        => h.copy(peers = newPeers)
    case fh: NodeState.FilterHeaderSync => fh.copy(peers = newPeers)
    case fs: NodeState.FilterSync       => fs.copy(peers = newPeers)
    case d: NodeState.DoneSyncing       => d.copy(peers = newPeers)
    case rm: NodeState.RemovePeers      => rm.copy(peers = newPeers)
    case m: NodeState.MisbehavingPeer   => m.copy(peers = newPeers)
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
      peers: Set[Peer],
      waitingForDisconnection: Set[Peer])
      extends SyncNodeState

  case class FilterHeaderSync(
      syncPeer: Peer,
      peers: Set[Peer],
      waitingForDisconnection: Set[Peer])
      extends SyncNodeState

  case class FilterSync(
      syncPeer: Peer,
      peers: Set[Peer],
      waitingForDisconnection: Set[Peer])
      extends SyncNodeState

  case class MisbehavingPeer(
      badPeer: Peer,
      peers: Set[Peer],
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
      peers: Set[Peer],
      waitingForDisconnection: Set[Peer],
      isSyncing: Boolean)
      extends NodeState {
    require(
      peersToRemove.forall(rm => peers.exists(_ == rm)),
      s"peersToRemove must be subset of peers, peersToRemove=$peersToRemove peers=$peers")
  }

  /** State to indicate we are not currently syncing with a peer */
  case class DoneSyncing(peers: Set[Peer], waitingForDisconnection: Set[Peer])
      extends NodeState {
    override val isSyncing: Boolean = false
  }
}
