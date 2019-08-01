package org.bitcoins.chain.api

import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.BlockHeader

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.chain.config.ChainAppConfig

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
}
