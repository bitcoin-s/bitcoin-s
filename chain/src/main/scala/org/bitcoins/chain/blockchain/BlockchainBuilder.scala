package org.bitcoins.chain.blockchain

import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}

import scala.collection.mutable

/**
  *
  * @param blockHeaderDAO
  */
case class BlockchainBuilder(blockHeaderDAO: BlockHeaderDAO)
    extends mutable.Builder[BlockHeaderDb, Blockchain] {
  private val internal = Vector.newBuilder[BlockHeaderDb]

  override def result(): Blockchain = {
    Blockchain.fromHeaders(internal.result().reverse)
  }

  override def +=(blockHeaderDb: BlockHeaderDb): this.type = {
    internal.+=(blockHeaderDb)
    this
  }

  override def clear(): Unit = internal.clear()
}
