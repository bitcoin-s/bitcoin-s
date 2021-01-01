package org.bitcoins.testkit.chain.models

import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.protocol.blockchain.BlockHeader

/** A trait that contains a reorg scenario that looks like this
  *                       headerDb1
  *                    /
  * oldBestBlockHeader
  *                    \
  *                      headerDb2
  */
sealed trait ReorgFixture {
  def headerDb1: BlockHeaderDb
  def headerDb2: BlockHeaderDb
  def oldBestBlockHeader: BlockHeaderDb

  lazy val header1: BlockHeader = headerDb1.blockHeader
  lazy val header2: BlockHeader = headerDb2.blockHeader
}

case class ReorgFixtureChainApi(
    chainApi: ChainApi,
    headerDb1: BlockHeaderDb,
    headerDb2: BlockHeaderDb,
    oldBestBlockHeader: BlockHeaderDb)
    extends ReorgFixture

case class ReorgFixtureBlockHeaderDAO(
    blockHeaderDAO: BlockHeaderDAO,
    headerDb1: BlockHeaderDb,
    headerDb2: BlockHeaderDb,
    oldBestBlockHeader: BlockHeaderDb)
    extends ReorgFixture
