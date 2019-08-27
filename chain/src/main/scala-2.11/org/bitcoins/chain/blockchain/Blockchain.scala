package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.BlockHeaderDb

import scala.collection.{mutable, IndexedSeqLike}

/** @inheritdoc */
case class Blockchain(headers: Vector[BlockHeaderDb])
    extends IndexedSeqLike[BlockHeaderDb, Vector[BlockHeaderDb]]
    with BaseBlockChain {

  protected[blockchain] def compObjectfromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]): Blockchain =
    Blockchain.fromHeaders(headers)

  /** @inheritdoc */
  override def newBuilder: mutable.Builder[
    BlockHeaderDb,
    Vector[BlockHeaderDb]] = Vector.newBuilder[BlockHeaderDb]

  /** @inheritdoc */
  override def seq: IndexedSeq[BlockHeaderDb] = headers
}

object Blockchain extends BaseBlockChainCompObject {

  def fromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]): Blockchain =
    Blockchain(headers.toVector)
}
