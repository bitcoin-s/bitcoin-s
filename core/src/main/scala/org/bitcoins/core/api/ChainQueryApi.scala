package org.bitcoins.core.api

import org.bitcoins.core.crypto.DoubleSha256DigestBE
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
  }
}
