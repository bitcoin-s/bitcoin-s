package org.bitcoins.chain.api

import org.bitcoins.chain.api.ChainApi.{BlockHeight, BlockStamp}
import org.bitcoins.chain.models.{
  BlockHeaderDb,
  CompactFilterDb,
  CompactFilterHeaderDb
}
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.protocol.blockchain.BlockHeader

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.gcs.FilterHeader
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.gcs._
import org.bitcoins.core.number.UInt32

import scala.annotation.tailrec

/**
  * Entry api to the chain project for adding new things to our blockchain
  */
trait ChainApi {

  implicit private[chain] val chainConfig: ChainAppConfig

  /**
    * Adds a block header to our chain project
    * @param header
    * @return
    */
  def processHeader(header: BlockHeader)(
      implicit ec: ExecutionContext): Future[ChainApi] = {
    processHeaders(Vector(header))
  }

  /** Process all of the given headers and returns a new [[ChainApi chain api]]
    * that contains these headers. This method processes headers in the order
    * that they are given. If the headers are out of order, this method will fail
    * @param headers
    * @return
    */
  def processHeaders(headers: Vector[BlockHeader])(
      implicit ec: ExecutionContext): Future[ChainApi]

  /** Gets a [[org.bitcoins.chain.models.BlockHeaderDb]] from the chain's database */
  def getHeader(hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]]

  /**  Gets all [[org.bitcoins.chain.models.BlockHeaderDb]]s at a given height  */
  def getHeadersAtHeight(height: Int)(
      implicit ec: ExecutionContext): Future[Vector[BlockHeaderDb]]

  /** Gets the number of blocks in the database */
  def getBlockCount(implicit ec: ExecutionContext): Future[Int]

  /** Gets the hash of the block that is what we consider "best" */
  def getBestBlockHash(
      implicit ec: ExecutionContext): Future[DoubleSha256DigestBE]

  /** Gets the best block header we have */
  def getBestBlockHeader(
      implicit ec: ExecutionContext): Future[BlockHeaderDb] = {
    for {
      hash <- getBestBlockHash
      headerOpt <- getHeader(hash)
    } yield headerOpt match {
      case None =>
        throw new RuntimeException(
          s"We found best hash=${hash.hex} but could not retrieve the full header!!!")
      case Some(header) => header
    }
  }

  /**
    * Adds a compact filter header into the filter header chain and returns a new [[ChainApi chain api]]
    * that contains this header
    */
  def processFilterHeader(
      filterHeader: FilterHeader,
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {
    processFilterHeaders(Vector(filterHeader), blockHash)
  }

  /**
    * Process all of the given compact filter headers and returns a new [[ChainApi chain api]]
    * that contains these headers.
    */
  def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi]

  /**
    * Generates a block range in form of (startHeight, stopHash) by the given stop hash.
    */
  def nextHeaderBatchRange(stopHash: DoubleSha256DigestBE, batchSize: Int)(
      implicit ec: ExecutionContext): Future[Option[(Int, DoubleSha256Digest)]]

  /**
    * Generates a filter header range in form of (startHeight, stopHash) by the given stop hash.
    */
  def nextFilterHeaderBatchRange(
      stopHash: DoubleSha256DigestBE,
      batchSize: Int)(
      implicit ec: ExecutionContext): Future[Option[(Int, DoubleSha256Digest)]]

  /**
    * Adds a compact filter into the filter database.
    */
  def processFilter(message: CompactFilterMessage)(
      implicit ec: ExecutionContext): Future[ChainApi] =
    processFilters(Vector(message))

  /**
    * Process all of the given compact filters and returns a new [[ChainApi chain api]]
    * that contains these headers.
    */
  def processFilters(message: Vector[CompactFilterMessage])(
      implicit ec: ExecutionContext): Future[ChainApi]

  /**
    * Adds a compact filter header check point into the list of check points.
    */
  def processCheckpoint(
      filterHeaderHash: DoubleSha256DigestBE,
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {
    processCheckpoints(Vector(filterHeaderHash), blockHash)
  }

  /**
    * Process all ompact filter header check points.
    */
  def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi]

  /** Gets the number of compact filter headers in the database */
  def getFilterHeaderCount(implicit ec: ExecutionContext): Future[Int]

  /**
    * Looks up a compact filter header by its height.
    */
  def getFilterHeadersAtHeight(height: Int)(
      implicit ec: ExecutionContext): Future[Vector[CompactFilterHeaderDb]]

  /**
    * Looks up a compact filter header by its hash.
    */
  def getFilterHeader(blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[CompactFilterHeaderDb]]

  /**
    * Looks up a compact filter by its hash.
    */
  def getFilter(hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[CompactFilterDb]]

  /** Gets the number of compact filters in the database */
  def getFilterCount(implicit ec: ExecutionContext): Future[Int]

  /**
    * Looks up a compact filter by its height.
    */
  def getFiltersAtHeight(height: Int)(
      implicit ec: ExecutionContext): Future[Vector[CompactFilterDb]]

  /**
    * Iterates over the block filters in order to find filters that match to the given addresses
    *
    * @param addresses list of addresses
    * @param start start point (if empty it starts with the genesis block)
    * @param end end point (if empty it ends with the best tip)
    * @return a list of matching block hashes
    */
  def getMatchingBlocks(
      addresses: Vector[BitcoinAddress],
      start: Option[BlockStamp] = None,
      end: Option[BlockStamp] = None)(
      implicit ec: ExecutionContext): Future[Vector[DoubleSha256DigestBE]]

}

object ChainApi {

  object BlockStamp {
    implicit def implicitHeight(height: Int): BlockHeight = BlockHeight(height)
    implicit def implicitHeader(header: BlockHeaderDb): HeaderBlockStamp =
      HeaderBlockStamp(header)

    def height(blockStamp: BlockStamp): Int = blockStamp match {
      case blockHeight: BlockHeight => blockHeight.height
      case header: HeaderBlockStamp => header.height
      case x: BlockStamp =>
        throw new RuntimeException(s"Invalid block stamp: $x")
    }
  }

  /** This trait represents a point on blockchain, and is used to specify block ranges */
  sealed trait BlockStamp extends Ordered[BlockStamp]

  case class BlockHeight(height: Int) extends BlockStamp {
    override def compare(that: BlockStamp): Int = that match {
      case blockHeight: BlockHeight => height.compareTo(blockHeight.height)
      case header: HeaderBlockStamp => height.compareTo(header.height)
      case _: BlockStamp            => 1
    }
  }

  case class HeaderBlockStamp(header: BlockHeaderDb) extends BlockStamp {
    def time: UInt32 = header.time
    def height: Int = header.height
    override def compare(that: BlockStamp): Int = that match {
      case blockHeight: BlockHeight => height.compareTo(blockHeight.height)
      case header: HeaderBlockStamp => height.compareTo(header.height)
      case _: BlockStamp            => 1
    }
  }

}
