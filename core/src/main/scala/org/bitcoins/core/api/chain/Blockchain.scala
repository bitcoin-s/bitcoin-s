package org.bitcoins.core.api.chain

import org.bitcoins.core.api.chain.db.BlockHeaderDb

/** @inheritdoc */
case class Blockchain(headers: Vector[BlockHeaderDb]) extends BaseBlockChain {

  /** The median time of the last 11 headers, or of all of them when the chain
    * starts at genesis with fewer than 11, as in Bitcoin Core. None when fewer
    * than 11 headers are loaded and they do not reach genesis.
    */
  def getMedianTimePast: Option[Long] = {
    val window = headers.take(Blockchain.nMedianTimeSpan)
    if (window.length < Blockchain.nMedianTimeSpan && window.last.height != 0) {
      None
    } else {
      val sorted = window.map(_.time.toLong).sorted
      Some(sorted.apply(sorted.length / 2))
    }
  }

  def getMedianTimePast(header: BlockHeaderDb): Option[Long] = {
    val headerIndexOpt = headers.indexWhere(_.hashBE == header.hashBE) match {
      case -1  => None
      case idx => Some(idx)
    }
    headerIndexOpt match {
      case Some(headerIndex) =>
        val sorted = headers
          .slice(from = headerIndex,
                 until = headerIndex + Blockchain.nMedianTimeSpan)
        Blockchain.fromHeaders(sorted).getMedianTimePast
      case None =>
        None
    }
  }
}

object Blockchain extends BaseBlockChainCompObject {
  val minHeadersMTP = 5
  val nMedianTimeSpan: Int = 11
  override def fromHeaders(
      headers: scala.collection.immutable.Seq[BlockHeaderDb]
  ): Blockchain =
    Blockchain(headers.toVector)
}
