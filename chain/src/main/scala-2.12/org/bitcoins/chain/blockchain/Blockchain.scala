package org.bitcoins.chain.blockchain

import org.bitcoins.core.api.chain.db.BlockHeaderDb

import scala.collection.mutable

/** @inheritdoc */
case class Blockchain(headers: Vector[BlockHeaderDb]) extends BaseBlockChain {

  protected[blockchain] def compObjectFromHeaders(
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
