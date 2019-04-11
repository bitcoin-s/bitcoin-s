package org.bitcoins.chain.blockchain

import org.bitcoins.chain.util.ChainUnitTest
import org.bitcoins.testkit.chain.BlockHeaderHelper
import org.scalatest.FutureOutcome

class ChainHandlerTest extends ChainUnitTest {

  override type FixtureParam = ChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandler(test)

  behavior of "ChainHandler"

  it must "process a new valid block header, and then be able to fetch that header" in {
    chainHandler: ChainHandler =>
      val newValidHeader = BlockHeaderHelper.buildNextHeader(genesisHeaderDb)
      val processedHeaderF =
        chainHandler.processHeader(newValidHeader.blockHeader)

      val foundHeaderF =
        processedHeaderF.flatMap(_.getHeader(newValidHeader.hashBE))

      foundHeaderF.map(found => assert(found.get == newValidHeader))
  }

}
