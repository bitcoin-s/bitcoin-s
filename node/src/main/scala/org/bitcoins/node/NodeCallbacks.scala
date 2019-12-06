package org.bitcoins.node

import org.bitcoins.node.networking.peer.DataMessageHandler._

/**
  * Callbacks for responding to events in the SPV node.
  * The approriate callback is executed whenver the node receives
  * a `getdata` message matching it.
  *
  */
case class NodeCallbacks(
    onCompactFilterReceived: Seq[OnCompactFilterReceived] = Seq.empty,
    onTxReceived: Seq[OnTxReceived] = Seq.empty,
    onBlockReceived: Seq[OnBlockReceived] = Seq.empty,
    onMerkleBlockReceived: Seq[OnMerkleBlockReceived] = Seq.empty
) {

  def +(other: NodeCallbacks): NodeCallbacks = copy(
    onCompactFilterReceived = onCompactFilterReceived ++ other.onCompactFilterReceived,
    onTxReceived = onTxReceived ++ other.onTxReceived,
    onBlockReceived = onBlockReceived ++ other.onBlockReceived,
    onMerkleBlockReceived = onMerkleBlockReceived ++ other.onMerkleBlockReceived
  )
}

object NodeCallbacks {

  /** Constructs a set of callbacks that only acts on TX received */
  def onTxReceived(f: OnTxReceived): NodeCallbacks =
    NodeCallbacks(onTxReceived = Seq(f))

  /** Constructs a set of callbacks that only acts on block received */
  def onBlockReceived(f: OnBlockReceived): NodeCallbacks =
    NodeCallbacks(onBlockReceived = Seq(f))

  /** Constructs a set of callbacks that only acts on merkle block received */
  def onMerkleBlockReceived(f: OnMerkleBlockReceived): NodeCallbacks =
    NodeCallbacks(onMerkleBlockReceived = Seq(f))

  /** Constructs a set of callbacks that only acts on compact filter received */
  def onCompactFilterReceived(f: OnCompactFilterReceived): NodeCallbacks =
    NodeCallbacks(onCompactFilterReceived = Seq(f))

  /** Empty callbacks that does nothing with the received data */
  val empty: NodeCallbacks =
    NodeCallbacks(
      onTxReceived = Seq.empty,
      onBlockReceived = Seq.empty,
      onMerkleBlockReceived = Seq.empty,
      onCompactFilterReceived = Seq.empty
    )
}
