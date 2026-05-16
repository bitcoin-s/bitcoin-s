package org.bitcoins.testkit.chain

import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.api.chain.{ChainApi, ChainQueryApi, FilterSyncMarker}
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

case class StaticChainApi(
    bestHeader: BlockHeaderDb,
    blockCount: Int,
    filterCount: Int,
    filterHeaderCount: Int,
    syncing: Boolean,
    isIBDValue: Boolean,
    medianTimePast: Long
) extends ChainApi {

  override def processHeaders(headers: Vector[BlockHeader]): Future[ChainApi] =
    Future.successful(this)

  override def getHeader(
      hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] =
    Future.successful(if (hash == bestHeader.hashBE) Some(bestHeader) else None)

  override def getHeaders(hashes: Vector[DoubleSha256DigestBE])
      : Future[Vector[Option[BlockHeaderDb]]] =
    Future.successful(hashes.map(hash =>
      if (hash == bestHeader.hashBE) Some(bestHeader) else None))

  override def getHeadersAtHeight(height: Int): Future[Vector[BlockHeaderDb]] =
    Future.successful(
      if (height == bestHeader.height) Vector(bestHeader)
      else Vector.empty)

  override def getBlockCount(): Future[Int] = Future.successful(blockCount)

  override def getBestBlockHeader(): Future[BlockHeaderDb] =
    Future.successful(bestHeader)

  override def getBestChainTips(): Future[Vector[BlockHeaderDb]] =
    Future.successful(Vector(bestHeader))

  override def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE): Future[ChainApi] =
    Future.successful(this)

  override def nextBlockHeaderBatchRange(
      prevStopHash: DoubleSha256DigestBE,
      stopHash: DoubleSha256DigestBE,
      batchSize: Int): Future[Option[FilterSyncMarker]] =
    Future.successful(None)

  override def nextFilterHeaderBatchRange(
      stopBlockHash: DoubleSha256DigestBE,
      batchSize: Int,
      startHeightOpt: Option[Int]): Future[Option[FilterSyncMarker]] =
    Future.successful(None)

  override def processFilters(
      message: Vector[CompactFilterMessage]): Future[ChainApi] =
    Future.successful(this)

  override def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE): Future[ChainApi] =
    Future.successful(this)

  override def getFilterHeaderCount(): Future[Int] =
    Future.successful(filterHeaderCount)

  override def getFilterHeadersAtHeight(
      height: Int): Future[Vector[CompactFilterHeaderDb]] =
    Future.successful(Vector.empty)

  override def getBestFilterHeader(): Future[Option[CompactFilterHeaderDb]] =
    Future.successful(None)

  override def getBestFilter(): Future[Option[CompactFilterDb]] =
    Future.successful(None)

  override def getFilterHeader(
      blockHash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]] =
    Future.successful(None)

  override def getFilter(
      hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] =
    Future.successful(None)

  override def getFilterCount(): Future[Int] = Future.successful(filterCount)

  override def getFiltersAtHeight(
      height: Int): Future[Vector[CompactFilterDb]] =
    Future.successful(Vector.empty)

  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    Future.successful(bestHeader.height)

  override def getHeadersBetween(
      from: BlockHeaderDb,
      to: BlockHeaderDb): Future[Vector[BlockHeaderDb]] =
    Future.successful(Vector.empty)

  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
    Future.successful(
      if (blockHash == bestHeader.hashBE) Some(bestHeader.height) else None)

  override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
    Future.successful(bestHeader.hashBE)

  override def getNumberOfConfirmations(
      blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
    Future.successful(if (blockHash == bestHeader.hashBE) Some(0) else None)

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[ChainQueryApi.FilterResponse]] =
    Future.successful(Vector.empty)

  override def epochSecondToBlockHeight(time: Long): Future[Int] =
    Future.successful(bestHeader.height)

  override def getMedianTimePast(): Future[Long] =
    Future.successful(medianTimePast)

  override def isSyncing(): Future[Boolean] = Future.successful(syncing)

  override def isIBD(): Future[Boolean] = Future.successful(isIBDValue)

  override def isTipStale(): Future[Boolean] = Future.successful(false)

  override def setSyncing(value: Boolean): Future[ChainApi] =
    Future.successful(copy(syncing = value))

  override def setIBD(value: Boolean): Future[ChainApi] =
    Future.successful(copy(isIBDValue = value))
}
