package org.bitcoins.chain.blockchain

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.{ExecutionContext, Future}

/** An optimized version of [[ChainHandler]] that avoids database reads
  * for determining what the best block header is. This should be used
  * with care as it is possible the cached [[blockchains]] may be out of date!
  * Unless you know what you are doing, you should probably use [[ChainHandler]]
  */
case class ChainHandlerCached(
    override val blockHeaderDAO: BlockHeaderDAO,
    override val filterHeaderDAO: CompactFilterHeaderDAO,
    override val filterDAO: CompactFilterDAO,
    override val blockchains: Vector[Blockchain],
    override val blockFilterCheckpoints: Map[
      DoubleSha256DigestBE,
      DoubleSha256DigestBE])(implicit
    override val chainConfig: ChainAppConfig,
    executionContext: ExecutionContext)
    extends ChainHandler(blockHeaderDAO,
                         filterHeaderDAO,
                         filterDAO,
                         blockchains,
                         blockFilterCheckpoints) {

  /** Gets the best block header from the given [[blockchains]] parameter */
  override def getBestBlockHeader(): Future[BlockHeaderDb] = {
    Future {
      getBestBlockHeaderHelper(blockchains)
    }
  }
}

object ChainHandlerCached {

  def fromDatabase(
      blockHeaderDAO: BlockHeaderDAO,
      filterHeaderDAO: CompactFilterHeaderDAO,
      filterDAO: CompactFilterDAO)(implicit
      ec: ExecutionContext,
      chainConfig: ChainAppConfig): Future[ChainHandlerCached] = {
    val bestChainsF = blockHeaderDAO.getBlockchains()

    bestChainsF.map(chains =>
      new ChainHandlerCached(blockHeaderDAO = blockHeaderDAO,
                             filterHeaderDAO = filterHeaderDAO,
                             filterDAO = filterDAO,
                             blockchains = chains,
                             blockFilterCheckpoints = Map.empty))
  }
}
