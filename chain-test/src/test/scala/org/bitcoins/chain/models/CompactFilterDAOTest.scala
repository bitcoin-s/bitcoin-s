package org.bitcoins.chain.models

import org.bitcoins.testkit.chain.ChainDbUnitTest
import org.scalatest.FutureOutcome

class CompactFilterDAOTest extends ChainDbUnitTest {

  override type FixtureParam = CompactFilterDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withCompactFilterDAO(test)

  behavior of "CompactFilterDAO"

  it must "retrieve getBestFilter when there are no filters in the db" in {
    compactFilterDAO: CompactFilterDAO =>
      compactFilterDAO.getBestFilter
        .map(opt => assert(opt == None))
  }
}
