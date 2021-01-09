package org.bitcoins.core.api.chain

import org.bitcoins.core.api.chain.db.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

/**
  * Entry api to the chain project for adding new things to our blockchain
  */
trait ChainApi extends ChainQueryApi {

  /**
    * Adds a block header to our chain project
    * @param header
    * @return
    */
  def processHeader(header: BlockHeader): Future[ChainApi] = {
    processHeaders(Vector(header))
  }

  /** Process all of the given headers and returns a new [[ChainApi chain api]]
    * that contains these headers. This method processes headers in the order
    * that they are given. If the headers are out of order, this method will fail
    * @param headers
    * @return
    */
  def processHeaders(headers: Vector[BlockHeader]): Future[ChainApi]

  /** Gets a [[org.bitcoins.chain.models.BlockHeaderDb]] from the chain's database */
  def getHeader(hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]]

  /** Gets all [[org.bitcoins.chain.models.BlockHeaderDb]]s at a given height */
  def getHeadersAtHeight(height: Int): Future[Vector[BlockHeaderDb]]

  /** Gets the number of blocks in the database */
  def getBlockCount(): Future[Int]

//  /** Gets the hash of the block that is what we consider "best" */
//  override def getBestBlockHash: Future[DoubleSha256DigestBE]

  /** Gets the best block header we have */
  def getBestBlockHeader(): Future[BlockHeaderDb]

  /**
    * Adds a compact filter header into the filter header chain and returns a new [[ChainApi chain api]]
    * that contains this header
    */
  def processFilterHeader(
      filterHeader: FilterHeader,
      blockHash: DoubleSha256DigestBE): Future[ChainApi] = {
    processFilterHeaders(Vector(filterHeader), blockHash)
  }

  /**
    * Process all of the given compact filter headers and returns a new [[ChainApi chain api]]
    * that contains these headers.
    */
  def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE): Future[ChainApi]

  /**
    * Generates a block range in form of (startHeight, stopHash) by the given stop hash.
    */
  def nextBlockHeaderBatchRange(
      prevStopHash: DoubleSha256DigestBE,
      batchSize: Int): Future[Option[FilterSyncMarker]]

  /**
    * Generates a filter header range in form of (startHeight, stopHash) by the given stop hash.
    */
  def nextFilterHeaderBatchRange(
      prevStopHash: DoubleSha256DigestBE,
      batchSize: Int): Future[Option[FilterSyncMarker]]

  /**
    * Adds a compact filter into the filter database.
    */
  def processFilter(message: CompactFilterMessage): Future[ChainApi] =
    processFilters(Vector(message))

  /**
    * Process all of the given compact filters and returns a new [[ChainApi chain api]]
    * that contains these headers.
    */
  def processFilters(message: Vector[CompactFilterMessage]): Future[ChainApi]

  /**
    * Adds a compact filter header check point into the list of check points.
    */
  def processCheckpoint(
      filterHeaderHash: DoubleSha256DigestBE,
      blockHash: DoubleSha256DigestBE): Future[ChainApi] = {
    processCheckpoints(Vector(filterHeaderHash), blockHash)
  }

  /**
    * Process all ompact filter header check points.
    */
  def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE): Future[ChainApi]

  /** Gets the number of compact filter headers in the database */
  def getFilterHeaderCount(): Future[Int]

  /**
    * Looks up a compact filter header by its height.
    */
  def getFilterHeadersAtHeight(
      height: Int): Future[Vector[CompactFilterHeaderDb]]

  /** Finds the "best" filter header we have stored in our database
    * What this means in practice is the latest filter header we
    * have received from our peer.
    * Returns none if we have no filters in the database
    */
  def getBestFilterHeader(): Future[Option[CompactFilterHeaderDb]]

  /**
    * Looks up a compact filter header by its hash.
    */
  def getFilterHeader(
      blockHash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]]

  /**
    * Looks up a compact filter by its hash.
    */
  def getFilter(hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]]

  /** Gets the number of compact filters in the database */
  def getFilterCount(): Future[Int]

  /**
    * Looks up a compact filter by its height.
    */
  def getFiltersAtHeight(height: Int): Future[Vector[CompactFilterDb]]

  /** Returns the block height of the given block stamp */
  def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int]

  /** Fetchs the block headers between from and to (inclusive). */
  def getHeadersBetween(
      from: BlockHeaderDb,
      to: BlockHeaderDb): Future[Vector[BlockHeaderDb]]
}
