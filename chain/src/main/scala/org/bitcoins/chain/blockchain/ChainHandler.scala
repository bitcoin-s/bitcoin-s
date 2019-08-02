package org.bitcoins.chain.blockchain

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.BlockHeader

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.db.ChainVerificationLogger
import org.bitcoins.chain.validation.TipUpdateResult.BadNonce
import org.bitcoins.chain.validation.TipUpdateResult.BadPOW
import org.bitcoins.chain.validation.TipUpdateResult.BadPreviousBlockHash
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.chain.validation.TipUpdateResult

/**
  * Chain Handler is meant to be the reference implementation
  * of [[org.bitcoins.chain.api.ChainApi ChainApi]], this is the entry point in to the
  * chain project.
  */
case class ChainHandler(blockHeaderDAO: BlockHeaderDAO)(
    implicit private[chain] val chainConfig: ChainAppConfig
) extends ChainApi
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

  override def processHeader(header: BlockHeader)(
      implicit ec: ExecutionContext): Future[ChainHandler] = {
    logger.debug(
      s"Processing header=${header.hashBE.hex}, previousHash=${header.previousBlockHashBE.hex}")

    val blockchainUpdateF =
      Blockchain.connectTip(header, blockHeaderDAO)

    val newHandlerF = blockchainUpdateF.flatMap {
      case BlockchainUpdate.Successful(_, updatedHeader) =>
        //now we have successfully connected the header, we need to insert
        //it into the database
        val createdF = blockHeaderDAO.create(updatedHeader)
        createdF.map { header =>
          logger.debug(
            s"Connected new header to blockchain, height=${header.height} hash=${header.hashBE.hex}")
          ChainHandler(blockHeaderDAO)
        }
      case BlockchainUpdate.Failed(_, _, reason) =>
        val errMsg =
          s"Failed to add header to chain, header=${header.hashBE.hex} reason=${reason}"
        logger.warn(errMsg)
        // potential chain split happening, let's log what's going on
        logTipConnectionFailure(reason).flatMap { _ =>
          Future.failed(new RuntimeException(errMsg))
        }
    }

    blockchainUpdateF.failed.foreach { err =>
      logger.error(
        s"Failed to connect header=${header.hashBE.hex} err=${err.getMessage}")

    }

    newHandlerF
  }

  /** Logs a tip connection failure by querying local chain state
    * and comparing it to the received `TipUpdateResult`
    */
  private def logTipConnectionFailure(failure: TipUpdateResult.Failure)(
      implicit ec: ExecutionContext): Future[Unit] = {
    failure match {
      case _ @(_: BadPOW | _: BadNonce) =>
        // TODO: Log this in a meaningful way
        FutureUtil.unit
      case _: BadPreviousBlockHash =>
        blockHeaderDAO.chainTips.map { tips =>
          if (tips.length > 1) {
            logger.warn {
              s"We have multiple (${tips.length}) , competing chainTips=${tips
                .map(_.hashBE.hex)
                .mkString("[", ",", "]")}"
            }
          } else {
            logger.warn(
              s"We don't have competing chainTips. Most recent, valid header=${tips.head.hashBE.hex}")
          }
        }
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
    blockHeaderDAO.chainTips.map { tips =>
      val sorted = tips.sortBy(header => header.blockHeader.difficulty)
      val hash = sorted.head.hashBE
      logger.debug(s"getBestBlockHash result: hash=$hash")
      hash
    }
  }
}
