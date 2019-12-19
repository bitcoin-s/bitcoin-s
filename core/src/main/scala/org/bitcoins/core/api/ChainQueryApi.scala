package org.bitcoins.core.api

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.util.FutureUtil

import scala.concurrent.Future

/**
  * This trait provides methods to query various types of blockchain data.
  */
trait ChainQueryApi {

  /** Gets the height of the given block */
  def getBlockHeight(blockHash: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the hash of the block that is what we consider "best" */
  def getBestBlockHash(): Future[DoubleSha256DigestBE]

  /** Gets number of confirmations for the given block hash*/
  def getNumberOfConfirmations(
      blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]]

  /** Gets the number of compact filters in the database */
  def getFilterCount: Future[Int]

  /** Returns the block height of the given block stamp */
  def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int]

  def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[(GolombFilter, DoubleSha256DigestBE)]]
}

object ChainQueryApi {

  object NoOp extends ChainQueryApi {

    /** Gets the height of the given block */
    override def getBlockHeight(
        blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
      FutureUtil.none

    /** Gets the hash of the block that is what we consider "best" */
    override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
      Future.successful(DoubleSha256DigestBE.empty)

    /** Gets number of confirmations for the given block hash. It returns None of no block found */
    override def getNumberOfConfirmations(
        blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] =
      FutureUtil.none

    /** Returns the block height of the given block stamp */
    override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
      Future.successful(0)

    /** Gets the number of compact filters in the database */
    override def getFilterCount: Future[Int] = Future.successful(0)

    override def getFiltersBetweenHeights(
        startHeight: Int,
        endHeight: Int): Future[Vector[(GolombFilter, DoubleSha256DigestBE)]] =
      Future.successful(Vector.empty)
  }

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
