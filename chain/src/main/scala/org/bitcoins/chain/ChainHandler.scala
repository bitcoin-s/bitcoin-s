package org.bitcoins.chain

import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.blockchain.BlockchainUpdate
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDb}
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.DbConfig

import scala.concurrent.{ExecutionContext, Future}

sealed abstract class ChainHandler extends ChainApi with BitcoinSLogger {
  def dbConfig: DbConfig

  def chainParams: ChainParams

  implicit def ec: ExecutionContext

  private val blockHeaderDAO = BlockHeaderDAO(chainParams, dbConfig)

  def blockchain: Blockchain

  override def getHeader(
      hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] = {
    blockHeaderDAO.findByHash(hash)
  }

  override def processHeader(header: BlockHeader): Future[ChainHandler] = {
    val blockchainUpdate = blockchain.connectTip(header)

    val newHandlerF = blockchainUpdate match {
      case BlockchainUpdate.Successful(blockchain, updatedHeader) =>
        //now we have successfully connected the header, we need to insert
        //it into the database
        val createdF = blockHeaderDAO.create(updatedHeader)
        createdF.map(_ => ChainHandler(blockchain, chainParams, dbConfig))
      case BlockchainUpdate.Failed(blockchain, _, reason) =>
        val errMsg =
          s"Failed to add header to chain, header=${header.hashBE.hex} reason=${reason}"
        logger.warn(errMsg)
        val newHandler = ChainHandler(blockchain, chainParams, dbConfig)
        Future.successful(newHandler)
    }

    newHandlerF
  }

}

object ChainHandler {
  private case class ChainHandlerImpl(
      blockchain: Blockchain,
      chainParams: ChainParams,
      dbConfig: DbConfig)(override implicit val ec: ExecutionContext)
      extends ChainHandler

  def apply(
      blockchain: Blockchain,
      chainParams: ChainParams,
      dbConfig: DbConfig)(implicit ec: ExecutionContext): ChainHandler = {
    ChainHandlerImpl(blockchain, chainParams, dbConfig)(ec)
  }
}
