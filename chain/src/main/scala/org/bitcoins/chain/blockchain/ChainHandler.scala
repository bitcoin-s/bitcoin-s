package org.bitcoins.chain.blockchain

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSLogger

import scala.concurrent.{ExecutionContext, Future}

/**
  * Chain Handler is meant to be the reference implementation
  * of [[ChainApi]], this is the entry point in to the
  * chain project.
  */
case class ChainHandler(
    blockHeaderDAO: BlockHeaderDAO,
    chainConfig: ChainAppConfig)
    extends ChainApi
    with BitcoinSLogger {

  override def getBlockCount: Future[Long] = {
    blockHeaderDAO.maxHeight
  }

  override def getHeader(
      hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash)
  }

  override def processHeader(header: BlockHeader)(
      implicit ec: ExecutionContext): Future[ChainHandler] = {

    val blockchainUpdateF = Blockchain.connectTip(header, blockHeaderDAO)

    val newHandlerF = blockchainUpdateF.flatMap {
      case BlockchainUpdate.Successful(_, updatedHeader) =>
        //now we have successfully connected the header, we need to insert
        //it into the database
        val createdF = blockHeaderDAO.create(updatedHeader)
        createdF.map(_ => ChainHandler(blockHeaderDAO, chainConfig))
      case BlockchainUpdate.Failed(_, _, reason) =>
        val errMsg =
          s"Failed to add header to chain, header=${header.hashBE.hex} reason=${reason}"
        logger.warn(errMsg)
        Future.failed(new RuntimeException(errMsg))
    }

    blockchainUpdateF.failed.foreach { err =>
      logger.error(
        s"Failed to connect header=${header.hashBE.hex} err=${err.getMessage}")

    }

    newHandlerF
  }

  /**
    * @inheritdoc
    */
  override def getBestBlockHash(
      implicit ec: ExecutionContext): Future[DoubleSha256DigestBE] = {
    //naive implementation, this is looking for the tip with the _most_ proof of work
    //this does _not_ mean that it is on the chain that has the most work
    //TODO: Enhance this in the future to return the "heaviest" header
    //https://bitcoin.org/en/glossary/block-chain
    blockHeaderDAO.chainTips.map { tips =>
      val sorted = tips.sortBy(header => header.blockHeader.difficulty)
      sorted.head.hashBE
    }
  }
}
