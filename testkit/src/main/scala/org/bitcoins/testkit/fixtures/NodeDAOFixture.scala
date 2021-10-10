package org.bitcoins.testkit.fixtures

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{BroadcastAbleTransactionDAO, PeerDAO}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.{CachedBitcoinSAppConfig, NodeUnitTest}
import org.scalatest._

import scala.concurrent.Future

case class NodeDAOs(txDAO: BroadcastAbleTransactionDAO, peerDAO: PeerDAO)

/** Provides a fixture where all DAOs used by the node projects are provided */
trait NodeDAOFixture extends NodeUnitTest with CachedBitcoinSAppConfig {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl, Vector.empty)

  private lazy val daos = {
    val tx = BroadcastAbleTransactionDAO()
    val peerDao = PeerDAO()
    NodeDAOs(tx, peerDao)
  }

  final override type FixtureParam = NodeDAOs

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeFixture(build = () => {
                  for {
                    _ <- cachedChainConf.start()
                    _ <- cachedNodeConf.start()
                  } yield daos
                },
                destroy =
                  () => destroyAppConfig(cachedChainConf, cachedNodeConf))(test)
  }

  private def destroyAppConfig(
      chainConfig: ChainAppConfig,
      nodeConfig: NodeAppConfig): Future[Unit] = {

    for {
      _ <- nodeConfig.dropAll()
      _ <- nodeConfig.stop()
      _ <- chainConfig.dropAll()
      _ <- chainConfig.stop()
    } yield ()
  }

  override def afterAll(): Unit = {
    super[CachedBitcoinSAppConfig].afterAll()
    super[NodeUnitTest].afterAll()
  }
}
