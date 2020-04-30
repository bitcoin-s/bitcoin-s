package org.bitcoins.core.api

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.BlockStamp

import scala.concurrent.{ExecutionContext, Future}

/**
  * This trait provides methods to query various types of blockchain data.
  */
trait ChainQueryApi {

  import org.bitcoins.core.api.ChainQueryApi._

  /** Gets the height of the given block */
  def getBlockHeight(blockHash: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the hash of the block that is what we consider "best" */
  def getBestBlockHash(): Future[DoubleSha256DigestBE]

  def getBestHashBlockHeight()(implicit ec: ExecutionContext): Future[Int] =
    for {
      hash <- getBestBlockHash()
      heightOpt <- getBlockHeight(hash)
      _ = require(heightOpt.isDefined,
                  s"Best block hash must have a height! blockhash=$hash")
    } yield heightOpt.get

  /** Gets number of confirmations for the given block hash*/
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the number of compact filters in the database */
  def getFilterCount: Future[Int]

  /** Returns the block height of the given block stamp */
  def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int]

  def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[FilterResponse]]

  /** Gets the block height of the closest block to the given time */
  def epochSecondToBlockHeight(time: Long): Future[Int]

}

object ChainQueryApi {

  case class FilterResponse(
      compactFilter: GolombFilter,
      blockHash: DoubleSha256DigestBE,
      blockHeight: Int)

  sealed abstract class ChainException(message: String)
      extends RuntimeException(message)

  /**
    * [[ChainQueryApi]] cannot find a compact
    * filter or header by its filter hash
    */
  case class UnknownFilterHash(message: String) extends ChainException(message)

  /**
    * [[ChainQueryApi]] cannot find a blockchain
    * item by its block hash
    */
  case class UnknownBlockHash(message: String) extends ChainException(message)

  /**
    * [[ChainQueryApi]] cannot find a blockchain
    * item by its height
    */
  case class UnknownBlockHeight(message: String) extends ChainException(message)

  /**
    * [[ChainQueryApi]] tried to process multiple filters for the same block hash
    */
  case class DuplicateFilters(message: String) extends ChainException(message)

  /**
    * The given block range is invalid
    */
  case class InvalidBlockRange(message: String) extends ChainException(message)

}
