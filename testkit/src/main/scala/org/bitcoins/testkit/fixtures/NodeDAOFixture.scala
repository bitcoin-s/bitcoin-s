package org.bitcoins.testkit.fixtures

import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import org.bitcoins.testkit.node.NodeUnitTest
import org.scalatest._

case class NodeDAOs(txDAO: BroadcastAbleTransactionDAO)

/** Provides a fixture where all DAOs used by the node projects are provided */
trait NodeDAOFixture extends NodeUnitTest {
  private lazy val daos = {
    val tx = BroadcastAbleTransactionDAO()
    NodeDAOs(tx)
  }

  final override type FixtureParam = NodeDAOs

  def withFixture(test: OneArgAsyncTest): FutureOutcome =
    makeFixture(build = () =>
                  nodeConfig
                    .createAll()(executionContext)
                    .map(_ => daos),
                destroy = () =>
                  nodeConfig
                    .dropAll()(executionContext))(test)
}
