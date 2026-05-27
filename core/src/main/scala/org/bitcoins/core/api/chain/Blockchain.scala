package org.bitcoins.core.api.chain

import org.bitcoins.core.api.chain.db.BlockHeaderDb

/** @inheritdoc */
case class Blockchain(headers: Vector[BlockHeaderDb]) extends BaseBlockChain {
  def getMedianTimePast: Option[Long] = {
    if (headers.length < Blockchain.nMedianTimeSpan) {
      None
    } else {
      val sorted = headers
        .take(Blockchain.nMedianTimeSpan)
        .map(_.time.toLong)
        .sorted
      Some(sorted.apply(sorted.length / 2))
    }
  def getMedianTimePast(header: BlockHeaderDb): Long = {
    val headerIndexOpt = headers.indexWhere(_.hash == header.hash) match {
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
        throw new IllegalArgumentException(
          s"Header ${header.hash} not found in blockchain"
        )
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
