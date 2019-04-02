package org.bitcoins.chain

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.{Blockchain, BlockchainUpdate}
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.DbConfig

import scala.concurrent.{ExecutionContext, Future}

/**
  * Chain Handler is meant to be the reference implementation
  * of [[ChainApi]], this is the entry point in to the
  * chain project.
  */
case class ChainHandler(
    blockchain: Blockchain,
    chainParams: ChainParams,
    dbConfig: DbConfig)(implicit ec: ExecutionContext)
    extends ChainApi
    with BitcoinSLogger {

  private val blockHeaderDAO = BlockHeaderDAO(chainParams, dbConfig)

  override def getHeader(
      hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash)
  }

  override def processHeader(header: BlockHeader): Future[ChainHandler] = {
    val blockchainUpdateF = blockchain.connectTip(header)

    val newHandlerF = blockchainUpdateF.flatMap {
      case BlockchainUpdate.Successful(blockchain, updatedHeader) =>
        //now we have successfully connected the header, we need to insert
        //it into the database
        val createdF = blockHeaderDAO.create(updatedHeader)
        createdF.map(_ => ChainHandler(blockchain, chainParams, dbConfig))
      case BlockchainUpdate.Failed(_, _, reason) =>
        val errMsg =
          s"Failed to add header to chain, header=${header.hashBE.hex} reason=${reason}"
        logger.warn(errMsg)
        Future.failed(new RuntimeException(errMsg))
    }

    newHandlerF
  }

}
