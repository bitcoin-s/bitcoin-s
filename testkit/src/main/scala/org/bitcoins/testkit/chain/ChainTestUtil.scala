package org.bitcoins.testkit.chain

import org.bitcoins.chain.models.{BlockHeaderDb, BlockHeaderDbHelper}
import org.bitcoins.core.protocol.blockchain.{
  BlockHeader,
  RegTestNetChainParams
}

sealed abstract class ChainTestUtil {
  lazy val regTestChainParams: RegTestNetChainParams.type =
    RegTestNetChainParams
  lazy val regTestHeader: BlockHeader =
    regTestChainParams.genesisBlock.blockHeader
  lazy val regTestHeaderDb: BlockHeaderDb = {
    BlockHeaderDbHelper.fromBlockHeader(height = 0, bh = regTestHeader)
  }
}

object ChainTestUtil extends ChainTestUtil
