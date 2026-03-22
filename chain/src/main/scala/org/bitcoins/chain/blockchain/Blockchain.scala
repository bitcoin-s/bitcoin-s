package org.bitcoins.chain.blockchain

import org.bitcoins.core.api.chain.db.BlockHeaderDb

/** @inheritdoc */
case class Blockchain(headers: Vector[BlockHeaderDb]) extends BaseBlockChain {
  def getMedianTimePast: Long = {
//    require(headers.length > 4,
//            s"Need at least 5 headers to calculate MTP, got=${headers.length}")
    val nMedianTimeSpan: Int = 11
    val sorted = headers
      .take(nMedianTimeSpan)
      .map(_.time.toLong)
      .sorted
    sorted.apply(sorted.length / 2)

  }
}

object Blockchain extends BaseBlockChainCompObject {

  override def fromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]
  ): Blockchain =
    Blockchain(headers.toVector)
}
