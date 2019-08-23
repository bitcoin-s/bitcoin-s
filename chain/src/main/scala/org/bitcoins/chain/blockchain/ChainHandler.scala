package org.bitcoins.chain.blockchain

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.BlockHeader

import scala.concurrent.{ExecutionContext, Future}

/**
  * Chain Handler is meant to be the reference implementation
  * of [[org.bitcoins.chain.api.ChainApi ChainApi]], this is the entry point in to the
  * chain project.
  */
case class ChainHandler(
    blockHeaderDAO: BlockHeaderDAO,
    blockchains: Vector[Blockchain])(
    implicit private[chain] val chainConfig: ChainAppConfig)
    extends ChainApi
    with ChainVerificationLogger {

  override def getBlockCount(implicit ec: ExecutionContext): Future[Long] = {
    logger.debug(s"Querying for block count")
    blockHeaderDAO.maxHeight.map { height =>
      logger.debug(s"getBlockCount result: count=$height")
      height
    }
  }

  override def getHeader(hash: DoubleSha256DigestBE)(
      implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash).map { header =>
      logger.debug(s"Looking for header by hash=$hash")
      val resultStr = header
        .map(h => s"height=${h.height}, hash=${h.hashBE}")
        .getOrElse("None")
      logger.debug(s"getHeader result: $resultStr")
      header
    }
  }

  /** @inheritdoc */
  override def processHeaders(headers: Vector[BlockHeader])(
      implicit ec: ExecutionContext): Future[ChainApi] = {
    val blockchainUpdates: Vector[BlockchainUpdate] = {
      Blockchain.connectHeadersToChains(headers, blockchains)
    }

    val headersToBeCreated = {
      blockchainUpdates.flatMap(_.successfulHeaders).distinct
    }

    val chains = blockchainUpdates.map(_.blockchain)

    val createdF = blockHeaderDAO.createAll(headersToBeCreated)

    val newChainHandler =
      ChainHandler(blockHeaderDAO = blockHeaderDAO, blockchains = chains)

    createdF.map { _ =>
      newChainHandler
    }
  }

  /**
    * @inheritdoc
    */
  override def getBestBlockHash(
      implicit ec: ExecutionContext): Future[DoubleSha256DigestBE] = {
    logger.debug(s"Querying for best block hash")
    //naive implementation, this is looking for the tip with the _most_ proof of work
    //this does _not_ mean that it is on the chain that has the most work
    //TODO: Enhance this in the future to return the "heaviest" header
    //https://bitcoin.org/en/glossary/block-chain
    val groupedChains = blockchains.groupBy(_.tip.height)
    val maxHeight = groupedChains.keys.max
    val chains = groupedChains(maxHeight)

    val hashBE: DoubleSha256DigestBE = chains match {
      case Vector() =>
        val errMsg = s"Did not find blockchain with height $maxHeight"
        logger.error(errMsg)
        throw new RuntimeException(errMsg)
      case chain +: Vector() =>
        chain.tip.hashBE
      case chain +: rest =>
        logger.warn(
          s"We have multiple competing blockchains: ${(chain +: rest).map(_.tip.hashBE.hex).mkString(", ")}")
        chain.tip.hashBE
    }
    Future.successful(hashBE)
  }
}

object ChainHandler {

  /** Constructs a [[ChainHandler chain handler]] from the state in the database
    * This gives us the guaranteed latest state we have in the database
    * */
  def fromDatabase(blockHeaderDAO: BlockHeaderDAO)(
      implicit ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[ChainHandler] = {
    val bestChainsF = blockHeaderDAO.getBlockchains()

    bestChainsF.map(chains =>
      new ChainHandler(blockHeaderDAO = blockHeaderDAO, blockchains = chains))
  }

  def apply(blockHeaderDAO: BlockHeaderDAO, blockchains: Blockchain)(
      implicit chainConfig: ChainAppConfig): ChainHandler = {
    new ChainHandler(blockHeaderDAO, Vector(blockchains))
  }
}
