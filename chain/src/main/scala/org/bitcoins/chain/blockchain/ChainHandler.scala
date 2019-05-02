package org.bitcoins.chain.blockchain

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.db.ChainDbConfig
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.core.util.BitcoinSLogger

import scala.concurrent.{ExecutionContext, Future}

/**
  * Chain Handler is meant to be the reference implementation
  * of [[ChainApi]], this is the entry point in to the
  * chain project.
  */
case class ChainHandler(blockHeaderDAO: BlockHeaderDAO, chainAppConfig: ChainAppConfig)
    extends ChainApi
    with BitcoinSLogger {

  def chainParams: ChainParams = chainAppConfig.chain

  def dbConfig: ChainDbConfig = chainAppConfig.dbConfig

  override def getBlockCount: Future[Long] = {
    blockHeaderDAO.maxHeight
  }

  override def getHeader(
      hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash)
  }

  override def processHeader(header: BlockHeader)(implicit ec: ExecutionContext): Future[ChainHandler] = {

    val blockchainUpdateF = Blockchain.connectTip(header, blockHeaderDAO)

    val newHandlerF = blockchainUpdateF.flatMap {
      case BlockchainUpdate.Successful(_, updatedHeader) =>
        //now we have successfully connected the header, we need to insert
        //it into the database
        val createdF = blockHeaderDAO.create(updatedHeader)
        createdF.map(_ => ChainHandler(blockHeaderDAO, chainAppConfig))
      case BlockchainUpdate.Failed(_, _, reason) =>
        val errMsg =
          s"Failed to add header to chain, header=${header.hashBE.hex} reason=${reason}"
        logger.warn(errMsg)
        Future.failed(new RuntimeException(errMsg))
    }

    newHandlerF
  }
}
