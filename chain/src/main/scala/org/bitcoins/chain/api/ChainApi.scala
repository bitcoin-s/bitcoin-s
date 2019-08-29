package org.bitcoins.chain.api

import org.bitcoins.chain.models.{BlockHeaderDb, CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.protocol.blockchain.BlockHeader

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.gcs.{FilterHeader, GolombFilter}
import org.bitcoins.core.p2p.CompactFilterMessage

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
  def getHeadersByHeight(height: Int)(
    implicit ec: ExecutionContext): Future[Seq[BlockHeaderDb]]

  /** Gets n-th [[org.bitcoins.chain.models.BlockHeaderDb]] down the blockchain form a given block */
  def getNthHeader(hash: DoubleSha256DigestBE, count: Int)(
    implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]]

  /** Gets the number of blocks in the database */
  def getBlockCount(implicit ec: ExecutionContext): Future[Long]

  /** Gets the hash of the block that is what we consider "best" */
  def getBestBlockHash(
      implicit ec: ExecutionContext): Future[DoubleSha256DigestBE]

  /** Gets the best block header we have */
  def getBestBlockHeader(
      implicit ec: ExecutionContext): Future[BlockHeaderDb] = {
    for {
      hash <- getBestBlockHash
      headerOpt <- getHeader(hash)
    } yield
      headerOpt match {
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
      blockHash: DoubleSha256DigestBE,
      height: Int)(implicit ec: ExecutionContext): Future[ChainApi]

  /**
    * Process all of the given compact filter headers and returns a new [[ChainApi chain api]]
    * that contains these headers.
    */
  def processFilterHeaders(
      filterHeaders: Vector[FilterHeader],
      stopHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {

    filterHeaders.reverseIterator
      .foldLeft(Future.successful(stopHash)) { (blockHashF, filterHeader) =>
        for {
          blockHash <- blockHashF
          headerOpt <- getHeader(blockHash)
          header = headerOpt.getOrElse(
            throw new RuntimeException(s"Unknown block hash ${blockHash.hex}"))
          _ <- processFilterHeader(filterHeader, blockHash, header.height)
        } yield {
          header.previousBlockHashBE
        }
      }
      .map(_ => this)
  }

  /**
    * Generates a block range in form of (startHeight, stopHash) by the given stop hash.
    */
  def nextBatchRange(stopHash: DoubleSha256DigestBE, batchSize: Long)(implicit ec: ExecutionContext): Future[Option[(Int, DoubleSha256Digest)]]

  /**
    * Adds a compact filter into the filter database.
    */
  def processFilter(
      message: CompactFilterMessage,
      blockHash: DoubleSha256DigestBE)(implicit ec: ExecutionContext): Future[ChainApi]

  /**
    * Adds a compact filter header check point into the list of check points.
    */
  def processCheckpoint(
      filterHeaderHash: DoubleSha256DigestBE,
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi]

  /**
    * Process all ompact filter header check points.
    */
  def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {

    checkpoints.foldLeft(Future.successful(this)) { (api, checkpoint) =>
      api.flatMap(_.processCheckpoint(checkpoint, blockHash))
    }
  }

  /**
    * Returns the highest know compact filter header.
    */
  def getHighestFilterHeader(implicit ec: ExecutionContext): Future[Option[CompactFilterHeaderDb]]

  /**
    * Looks up a compact filter header by its hash.
    */
  def getFilterHeader(hash: DoubleSha256DigestBE)(implicit ec: ExecutionContext): Future[Option[FilterHeader]]

  /**
    * Returns the highest know compact filter.
    */
  def getHighestFilter(implicit ec: ExecutionContext): Future[Option[CompactFilterDb]]

  /**
    * Looks up a compact filter by its hash.
    */
  def getFilter(hash: DoubleSha256DigestBE)(implicit ec: ExecutionContext): Future[Option[CompactFilterDb]]

}
