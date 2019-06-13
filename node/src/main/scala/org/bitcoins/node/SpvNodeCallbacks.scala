package org.bitcoins.node

import org.bitcoins.node.networking.peer.DataMessageHandler._

/**
  * Callbacks for responding to events in the SPV node.
  * The approriate callback is executed whenver the node receives
  * a `getdata` message matching it.
  *
  */
case class SpvNodeCallbacks(
    onTxReceived: Seq[OnTxReceived] = Seq.empty,
    onBlockReceived: Seq[OnBlockReceived] = Seq.empty,
    onMerkleBlockReceived: Seq[OnMerkleBlockReceived] = Seq.empty
)

object SpvNodeCallbacks {

  /** Empty callbacks that does nothing with the received data */
  val empty: SpvNodeCallbacks =
    SpvNodeCallbacks(
      onTxReceived = Seq.empty,
      onBlockReceived = Seq.empty,
      onMerkleBlockReceived = Seq.empty
    )
}
