package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}

import scala.collection.mutable

case class BlockchainBuilder(blockHeaderDAO: BlockHeaderDAO) extends mutable.ReusableBuilder[BlockHeaderDb, Blockchain] {
  private val internal = Vector.newBuilder[BlockHeaderDb]


  override def result(): Blockchain = {
    Blockchain.fromHeaders(internal.result(), blockHeaderDAO)
  }

  override def +=(blockHeaderDb: BlockHeaderDb): this.type = {
    internal.+=(blockHeaderDb)
    this
  }


  override def clear(): Unit = internal.clear()
}
