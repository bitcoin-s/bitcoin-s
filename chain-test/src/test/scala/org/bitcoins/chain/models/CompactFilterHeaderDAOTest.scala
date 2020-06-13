package org.bitcoins.chain.models

import org.bitcoins.testkit.chain.ChainDbUnitTest
import org.scalatest.FutureOutcome

class CompactFilterHeaderDAOTest extends ChainDbUnitTest {

  override type FixtureParam = CompactFilterHeaderDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withCompactFilterHeaderDAO(test)

  behavior of "CompactFilterHeaderDAO"

  it must "get the best filter header with a table with zero rows in it" in {
    filterHeaderDAO =>
      filterHeaderDAO.getBestFilterHeader.map { opt =>
        assert(opt == None)
      }
  }
}
