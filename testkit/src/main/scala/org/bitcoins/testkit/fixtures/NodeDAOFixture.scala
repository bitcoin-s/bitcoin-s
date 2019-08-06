package org.bitcoins.testkit.fixtures

import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import org.bitcoins.testkit.node.NodeUnitTest
import org.scalatest._
import slick.jdbc.SQLiteProfile

case class NodeDAOs(txDAO: BroadcastAbleTransactionDAO)

/** Provides a fixture where all DAOs used by the node projects are provided */
trait NodeDAOFixture extends fixture.AsyncFlatSpec with NodeUnitTest {
  private lazy val daos = {
    val tx = BroadcastAbleTransactionDAO(SQLiteProfile)
    NodeDAOs(tx)
  }

  final override type FixtureParam = NodeDAOs

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(
      build = () => NodeDbManagement.createAll()(nodeConfig, ec).map(_ => daos),
      destroy = () => NodeDbManagement.dropAll()(nodeConfig, ec))(test)
}
