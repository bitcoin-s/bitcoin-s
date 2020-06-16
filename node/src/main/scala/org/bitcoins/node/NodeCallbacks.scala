package org.bitcoins.node

import org.bitcoins.core.api.{Callback, Callback2, CallbackHandler}
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.blockchain.{Block, BlockHeader, MerkleBlock}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256Digest
import org.slf4j.Logger

import scala.concurrent.{ExecutionContext, Future}

/**
  * Callbacks for responding to events in the node.
  * The appropriate callback is executed whenever the node receives
  * a `getdata` message matching it.
  */
trait NodeCallbacks {

  def onCompactFiltersReceived: CallbackHandler[
    Vector[(DoubleSha256Digest, GolombFilter)],
    OnCompactFiltersReceived]

  def onTxReceived: CallbackHandler[Transaction, OnTxReceived]

  def onBlockReceived: CallbackHandler[Block, OnBlockReceived]

  def onMerkleBlockReceived: CallbackHandler[
    (MerkleBlock, Vector[Transaction]),
    OnMerkleBlockReceived]

  def onBlockHeadersReceived: CallbackHandler[
    Vector[BlockHeader],
    OnBlockHeadersReceived]

  def +(other: NodeCallbacks): NodeCallbacks

  def executeOnTxReceivedCallbacks(logger: Logger, tx: Transaction)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onTxReceived.execute(logger, tx)
  }

  def executeOnBlockReceivedCallbacks(logger: Logger, block: Block)(
      implicit ec: ExecutionContext): Future[Unit] = {
    onBlockReceived.execute(logger, block)
  }

  def executeOnMerkleBlockReceivedCallbacks(
      logger: Logger,
      merkleBlock: MerkleBlock,
      txs: Vector[Transaction])(implicit ec: ExecutionContext): Future[Unit] = {
    onMerkleBlockReceived.execute(logger, (merkleBlock, txs))
  }

  def executeOnCompactFiltersReceivedCallbacks(
      logger: Logger,
      blockFilters: Vector[(DoubleSha256Digest, GolombFilter)])(
      implicit ec: ExecutionContext): Future[Unit] = {
    onCompactFiltersReceived.execute(logger, blockFilters)
  }

  def executeOnBlockHeadersReceivedCallbacks(
      logger: Logger,
      headers: Vector[BlockHeader])(
      implicit ec: ExecutionContext): Future[Unit] = {
    onBlockHeadersReceived.execute(logger, headers)
  }
}

/** Callback for handling a received block */
trait OnBlockReceived extends Callback[Block]

/** Callback for handling a received Merkle block with its corresponding TXs */
trait OnMerkleBlockReceived extends Callback2[MerkleBlock, Vector[Transaction]]

/** Callback for handling a received transaction */
trait OnTxReceived extends Callback[Transaction]

/** Callback for handling a received compact block filter */
trait OnCompactFiltersReceived
    extends Callback[Vector[(DoubleSha256Digest, GolombFilter)]]

/** Callback for handling a received block header */
trait OnBlockHeadersReceived extends Callback[Vector[BlockHeader]]

object NodeCallbacks {

  // Use Impl pattern here to enforce the correct names on the CallbackHandlers
  private case class NodeCallbacksImpl(
      onCompactFiltersReceived: CallbackHandler[
        Vector[(DoubleSha256Digest, GolombFilter)],
        OnCompactFiltersReceived],
      onTxReceived: CallbackHandler[Transaction, OnTxReceived],
      onBlockReceived: CallbackHandler[Block, OnBlockReceived],
      onMerkleBlockReceived: CallbackHandler[
        (MerkleBlock, Vector[Transaction]),
        OnMerkleBlockReceived],
      onBlockHeadersReceived: CallbackHandler[
        Vector[BlockHeader],
        OnBlockHeadersReceived]
  ) extends NodeCallbacks {

    override def +(other: NodeCallbacks): NodeCallbacks =
      copy(
        onCompactFiltersReceived = onCompactFiltersReceived ++ other.onCompactFiltersReceived,
        onTxReceived = onTxReceived ++ other.onTxReceived,
        onBlockReceived = onBlockReceived ++ other.onBlockReceived,
        onMerkleBlockReceived = onMerkleBlockReceived ++ other.onMerkleBlockReceived,
        onBlockHeadersReceived = onBlockHeadersReceived ++ other.onBlockHeadersReceived
      )
  }

  /** Constructs a set of callbacks that only acts on TX received */
  def onTxReceived(f: OnTxReceived): NodeCallbacks =
    NodeCallbacks(onTxReceived = Vector(f))

  /** Constructs a set of callbacks that only acts on block received */
  def onBlockReceived(f: OnBlockReceived): NodeCallbacks =
    NodeCallbacks(onBlockReceived = Vector(f))

  /** Constructs a set of callbacks that only acts on merkle block received */
  def onMerkleBlockReceived(f: OnMerkleBlockReceived): NodeCallbacks =
    NodeCallbacks(onMerkleBlockReceived = Vector(f))

  /** Constructs a set of callbacks that only acts on compact filter received */
  def onCompactFilterReceived(f: OnCompactFiltersReceived): NodeCallbacks =
    NodeCallbacks(onCompactFiltersReceived = Vector(f))

  /** Constructs a set of callbacks that only acts on block headers received */
  def onBlockHeadersReceived(f: OnBlockHeadersReceived): NodeCallbacks =
    NodeCallbacks(onBlockHeadersReceived = Vector(f))

  /** Empty callbacks that does nothing with the received data */
  val empty: NodeCallbacks =
    NodeCallbacks(Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty)

  def apply(
      onCompactFiltersReceived: Vector[OnCompactFiltersReceived] = Vector.empty,
      onTxReceived: Vector[OnTxReceived] = Vector.empty,
      onBlockReceived: Vector[OnBlockReceived] = Vector.empty,
      onMerkleBlockReceived: Vector[OnMerkleBlockReceived] = Vector.empty,
      onBlockHeadersReceived: Vector[OnBlockHeadersReceived] = Vector.empty): NodeCallbacks = {
    NodeCallbacksImpl(
      onCompactFiltersReceived =
        CallbackHandler[Vector[(DoubleSha256Digest, GolombFilter)],
                        OnCompactFiltersReceived]("onCompactFilterReceived",
                                                  onCompactFiltersReceived),
      onTxReceived = CallbackHandler[Transaction, OnTxReceived]("onTxReceived",
                                                                onTxReceived),
      onBlockReceived =
        CallbackHandler[Block, OnBlockReceived]("onBlockReceived",
                                                onBlockReceived),
      onMerkleBlockReceived =
        CallbackHandler[(MerkleBlock, Vector[Transaction]),
                        OnMerkleBlockReceived]("onCompactFilterReceived",
                                               onMerkleBlockReceived),
      onBlockHeadersReceived =
        CallbackHandler[Vector[BlockHeader], OnBlockHeadersReceived](
          "onCompactFilterReceived",
          onBlockHeadersReceived)
    )
  }
}
