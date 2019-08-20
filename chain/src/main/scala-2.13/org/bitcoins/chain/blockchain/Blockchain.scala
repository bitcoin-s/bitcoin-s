package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.BlockHeaderDb

import scala.collection.immutable.IndexedSeq

/** @inheritdoc */
case class Blockchain(headers: Vector[BlockHeaderDb])
    extends IndexedSeq[BlockHeaderDb]
    with BaseBlockChain {

  protected[blockchain] def compObjectfromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]) =
    Blockchain.fromHeaders(headers)

  /** @inheritdoc */
  override def seq = this

}

object Blockchain extends BaseBlockChainCompObject {

  override def fromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]): Blockchain =
    Blockchain(headers.toVector)
}
