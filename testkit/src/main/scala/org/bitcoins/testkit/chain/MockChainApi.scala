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
import org.bitcoins.testkitcore.chain.ChainTestUtil

import scala.concurrent.Future

object MockChainApi extends ChainApi {

  override def processHeaders(
      headers: Vector[BlockHeader]
  ): Future[ChainApi] =
    Future.successful(this)

  override def getHeader(
      hash: DoubleSha256DigestBE
  ): Future[Option[BlockHeaderDb]] =
    Future.successful(None)

  override def getHeaders(
      hashes: Vector[DoubleSha256DigestBE]
  ): Future[Vector[Option[BlockHeaderDb]]] = {
    Future.successful(Vector.fill(hashes.length)(None))
  }

  override def getHeadersAtHeight(
      height: Int
  ): Future[Vector[BlockHeaderDb]] =
    Future.successful(Vector.empty)

  override def getBlockCount(): Future[Int] = Future.successful(0)

  override def getBestBlockHeader(): Future[BlockHeaderDb] =
    Future.successful(ChainTestUtil.regTestGenesisHeaderDb)

  override def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE
  ): Future[ChainApi] =
    Future.successful(this)

  override def nextBlockHeaderBatchRange(
      prevStopHash: DoubleSha256DigestBE,
      stopHash: DoubleSha256DigestBE,
      batchSize: Int
  ): Future[Option[FilterSyncMarker]] =
    Future.successful(None)

  override def nextFilterHeaderBatchRange(
      stopBlockHash: DoubleSha256DigestBE,
      batchSize: Int,
      startHeightOpt: Option[Int]
  ): Future[Option[FilterSyncMarker]] =
    Future.successful(None)

  override def processFilters(
      message: Vector[CompactFilterMessage]
  ): Future[ChainApi] =
    Future.successful(this)

  override def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE
  ): Future[ChainApi] =
    Future.successful(this)

  override def getFilterHeaderCount(): Future[Int] = Future.successful(0)

  override def getFilterHeadersAtHeight(
      height: Int
  ): Future[Vector[CompactFilterHeaderDb]] =
    Future.successful(Vector.empty)

  override def getBestFilterHeader(): Future[Option[CompactFilterHeaderDb]] =
    Future.successful(None)

  override def getBestFilter(): Future[Option[CompactFilterDb]] = {
    Future.successful(None)
  }

  override def getFilterHeader(
      blockHash: DoubleSha256DigestBE
  ): Future[Option[CompactFilterHeaderDb]] = Future.successful(None)

  override def getFilter(
      hash: DoubleSha256DigestBE
  ): Future[Option[CompactFilterDb]] =
    Future.successful(None)

  override def getFilterCount(): Future[Int] = Future.successful(0)

  override def getFiltersAtHeight(
      height: Int
  ): Future[Vector[CompactFilterDb]] =
    Future.successful(Vector.empty)

  override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
    Future.successful(0)

  override def getHeadersBetween(
      from: BlockHeaderDb,
      to: BlockHeaderDb
  ): Future[Vector[BlockHeaderDb]] =
    Future.successful(Vector.empty)

  override def getBlockHeight(
      blockHash: DoubleSha256DigestBE
  ): Future[Option[Int]] =
    Future.successful(None)

  override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
    Future.successful(DoubleSha256DigestBE.empty)

  override def getBestChainTips(): Future[Vector[BlockHeaderDb]] = {
    Future.successful(Vector.empty)
  }

  override def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE
  ): Future[Option[Int]] =
    Future.successful(None)

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int
  ): Future[Vector[ChainQueryApi.FilterResponse]] =
    Future.successful(Vector.empty)

  override def epochSecondToBlockHeight(time: Long): Future[Int] =
    Future.successful(0)

  /** calculates the median time passed */
  override def getMedianTimePast(): Future[Long] =
    Future.successful(0L)

  override def isSyncing(): Future[Boolean] = Future.successful(false)

  override def isIBD(): Future[Boolean] = Future.successful(false)

  override def isTipStale(): Future[Boolean] = {
    Future.successful(false)
  }

  override def setSyncing(value: Boolean): Future[ChainApi] =
    Future.successful(this)

  override def setIBD(value: Boolean): Future[ChainApi] = {
    Future.successful(this)
  }
}
