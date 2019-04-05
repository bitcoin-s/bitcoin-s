package org.bitcoins.chain.blockchain

import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.testkit.chain.BlockHeaderHelper

class ChainHandlerTest extends ChainUnitTest {

  behavior of "ChainHandler"

  it must "process a new valid block header, and then be able to fetch that header" in withChainHandler {
    case chainHandler: ChainHandler =>
      val newValidHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val processedHeaderF =
        chainHandler.processHeader(newValidHeader.blockHeader)

      val foundHeaderF =
        processedHeaderF.flatMap(_.getHeader(newValidHeader.hashBE))

      foundHeaderF.map(found => assert(found.get == newValidHeader))
  }

}
