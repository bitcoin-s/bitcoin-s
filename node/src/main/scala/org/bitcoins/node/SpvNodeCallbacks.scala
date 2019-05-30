package org.bitcoins.node

import org.bitcoins.node.networking.peer.DataMessageHandler._

/**
  * Callbacks for responding to events in the SPV node.
  * The approriate callback is executed whenver the node receives
  * a `getdata` message matching it.
  *
  */
case class SpvNodeCallbacks(
    onTxReceived: OnTxReceived = noopTxReceived,
    onBlockReceived: OnBlockReceived = noopBlockReceived,
    onMerkleBlockReceived: OnMerkleBlockReceived = noopMerkleBlockReceived,
)
