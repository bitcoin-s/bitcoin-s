package org.bitcoins.testkit.fixtures

import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.BroadcastAbleTransactionDAO
import org.bitcoins.testkit.node.NodeUnitTest
import org.scalatest._

import scala.concurrent.Future

case class NodeDAOs(txDAO: BroadcastAbleTransactionDAO)

/** Provides a fixture where all DAOs used by the node projects are provided */
trait NodeDAOFixture extends NodeUnitTest {

  implicit protected lazy val nodeConfig: NodeAppConfig = config.nodeConf

  private lazy val daos = {
    val tx = BroadcastAbleTransactionDAO()
    NodeDAOs(tx)
  }

  final override type FixtureParam = NodeDAOs

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => {
                  nodeConfig
                    .start()
                    .map(_ => daos)
                },
                destroy = () => destroyAppConfig(nodeConfig))(test)
  }

  private def destroyAppConfig(nodeConfig: NodeAppConfig): Future[Unit] = {
    for {
      _ <- nodeConfig.dropAll()
      _ <- nodeConfig.stop()
    } yield ()
  }
}
