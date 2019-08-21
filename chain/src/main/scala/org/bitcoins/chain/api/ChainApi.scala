package org.bitcoins.chain.api

import org.bitcoins.chain.models.{BlockHeaderDb, CompactFilterDb, CompactFilterHeaderDb}
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.protocol.blockchain.BlockHeader

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.gcs.{FilterHeader, GolombFilter}

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
      implicit ec: ExecutionContext): Future[ChainApi]

  /** Process all of the given headers and returns a new [[ChainApi chain api]]
    * that contains these headers. This method processes headers in the order
    * that they are given. If the headers are out of order, this method will fail
    * @param headers
    * @return
    */
  def processHeaders(headers: Vector[BlockHeader])(
      implicit ec: ExecutionContext): Future[ChainApi] = {
    headers.foldLeft(Future.successful(this)) {
      case (chainF, header) =>
        chainF.flatMap(_.processHeader(header))
    }
  }

  /** Get's a [[org.bitcoins.chain.models.BlockHeaderDb]] from the chain's database */
  def getHeader(hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]]

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

  def processFilterHeader(
      filterHeader: FilterHeader,
      blockHash: DoubleSha256DigestBE,
      height: Int)(implicit ec: ExecutionContext): Future[ChainApi]

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

  def processFilter(
      golombFilter: GolombFilter,
      blockHash: DoubleSha256DigestBE)(implicit ec: ExecutionContext): Future[ChainApi]

  def processCheckpoint(
      filterHeaderHash: DoubleSha256DigestBE,
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi]

  def processCheckpoints(
      checkpoints: Vector[DoubleSha256DigestBE],
      blockHash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[ChainApi] = {

    checkpoints.foldLeft(Future.successful(this)) { (api, checkpoint) =>
      api.flatMap(_.processCheckpoint(checkpoint, blockHash))
    }
  }

  def getHighestFilterHeader(implicit ec: ExecutionContext): Future[Option[CompactFilterHeaderDb]]

  def getFilterHeader(hash: DoubleSha256DigestBE)(implicit ec: ExecutionContext): Future[Option[FilterHeader]]

  def getHighestFilter(implicit ec: ExecutionContext): Future[Option[CompactFilterDb]]

  def getFilter(hash: DoubleSha256DigestBE)(implicit ec: ExecutionContext): Future[Option[CompactFilterDb]]

}
