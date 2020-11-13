package org.bitcoins.chain.blockchain

import org.bitcoins.testkit.chain.{ChainDbUnitTest, ChainUnitTest}
import org.bitcoins.testkit.chain.fixture.ChainFixtureTag
import org.scalatest.FutureOutcome

class ChainHandlerCachedTest extends ChainDbUnitTest {
  override type FixtureParam = ChainHandlerCached

  override val defaultTag: ChainFixtureTag =
    ChainFixtureTag.GenesisChainHandlerCachedWithFilter

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainHandlerCachedGenesisFilter(test)

  behavior of "ChainHandlerCached"

  it must "throw an error when we have no chains" in {
    chainHandlerCached: ChainHandlerCached =>
      val handler = chainHandlerCached.copy(blockchains = Vector.empty)

      recoverToSucceededIf[RuntimeException] {
        handler
          .getBestBlockHeader()
      }
  }

  it must "get best filter header with zero blockchains in memory" in {
    chainHandlerCached: ChainHandlerCached =>
      val noChainsChainHandler =
        chainHandlerCached.copy(blockchains = Vector.empty)

      for {
        filterHeaderOpt <- noChainsChainHandler.getBestFilterHeader()
      } yield {
        assert(filterHeaderOpt.isDefined)
        assert(filterHeaderOpt.get == ChainUnitTest.genesisFilterHeaderDb)
      }
  }

}
