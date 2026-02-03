package org.bitcoins.testkit.fixtures

import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{BroadcastAbleTransactionDAO, PeerDAO}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.{NodeUnitTest}
import org.scalatest._

import scala.concurrent.Future

case class NodeDAOs(
    txDAO: BroadcastAbleTransactionDAO,
    peerDAO: PeerDAO,
    nodeAppConfig: NodeAppConfig
)

/** Provides a fixture where all DAOs used by the node projects are provided */
trait NodeDAOFixture extends NodeUnitTest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(
      () => pgUrl(),
      Vector.empty
    )

  final override type FixtureParam = NodeDAOs

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[NodeDAOs](
      build = () => {
        val config = getFreshConfig
        val nodeConf = config.nodeConf
        for {
          _ <- nodeConf.start()
        } yield {
          val tx =
            BroadcastAbleTransactionDAO()(using nodeConf, executionContext)
          val peerDao = PeerDAO()(using nodeConf, executionContext)
          NodeDAOs(tx, peerDao, nodeConf)
        }
      },
      destroy = { case dao: NodeDAOs =>
        destroyAppConfig(dao.nodeAppConfig)
      }
    )(test)
  }

  private def destroyAppConfig(nodeConfig: NodeAppConfig): Future[Unit] = {
    nodeConfig.clean()
    for {
      _ <- nodeConfig.stop()
    } yield ()
  }

  override def afterAll(): Unit = {
    super[NodeUnitTest].afterAll()
  }
}
