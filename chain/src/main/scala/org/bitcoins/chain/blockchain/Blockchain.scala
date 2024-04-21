package org.bitcoins.chain.blockchain

import org.bitcoins.core.api.chain.db.BlockHeaderDb

/** @inheritdoc */
case class Blockchain(headers: Vector[BlockHeaderDb]) extends BaseBlockChain {

  protected[blockchain] def compObjectFromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]) =
    Blockchain.fromHeaders(headers)

  override def find(p: BlockHeaderDb => Boolean): Option[BlockHeaderDb] =
    headers.find(p)

}

object Blockchain extends BaseBlockChainCompObject {

  override def fromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]): Blockchain =
    Blockchain(headers.toVector)
}
