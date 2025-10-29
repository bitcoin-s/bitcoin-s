package org.bitcoins.node.networking

import com.typesafe.config.ConfigFactory
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.node.NeutrinoNode
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest
}
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import scala.concurrent.duration.{DurationInt, FiniteDuration}

class ReConnectionTest extends NodeTestWithCachedBitcoindNewest {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(
      () => pgUrl(),
      Vector.empty
    )

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome <- withNeutrinoNodeUnstarted(test, bitcoind)(
        system,
        getFreshConfig
      ).toFuture
    } yield outcome

    new FutureOutcome(f)
  }

  behavior of "ReConnectionTest"

  it must "disconnect a peer after a period of inactivity" in {
    (nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind) =>
      val bitcoind = nodeConnectedWithBitcoind.bitcoind
      val timeout = 5.seconds
      val startedF =
        getSmallHealthCheckNeutrinoNode(nodeConnectedWithBitcoind.node, timeout)
      for {
        started <- startedF
        _ <- NodeTestUtil.awaitConnectionCount(
          node = started,
          expectedConnectionCount = 1
        )
        // let sync occur so we aren't receiving blockchain data over the network
        _ <- NodeTestUtil.awaitAllSync(started, bitcoind)
        // wait until there is a timeout for inactivity
        _ <- AsyncUtil.nonBlockingSleep(timeout)
        _ <- NodeTestUtil.awaitConnectionCount(
          node = started,
          expectedConnectionCount = 0
        )
        _ <- started.stop()
        _ <- started.nodeAppConfig.stop()
      } yield {
        succeed
      }
  }

  it must "reconnect a peer when inactivity checks run and we have 0 peers" in {
    (nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind) =>
      // see: https://github.com/bitcoin-s/bitcoin-s/issues/5162
      val bitcoind = nodeConnectedWithBitcoind.bitcoind
      val timeout = 5.second
      val startedF =
        getSmallHealthCheckNeutrinoNode(nodeConnectedWithBitcoind.node, timeout)
      for {
        started <- startedF
        _ <- NodeTestUtil.awaitConnectionCount(started, 1)
        // explicitly disconnect it
        bitcoindPeer <- NodeTestUtil.getBitcoindPeer(bitcoind)
        _ <- started.peerManager.disconnectPeer(bitcoindPeer)
        // wait until we have zero connections
        _ <- NodeTestUtil.awaitConnectionCount(started, 0)

        // wait until there is a timeout for inactivity
        _ <- NodeTestUtil.awaitConnectionCount(started, 1)
        _ <- started.peerManager.isConnected(bitcoindPeer)
        _ <- started.stop()
        _ <- started.nodeAppConfig.stop()
      } yield {
        succeed
      }
  }

  private def getSmallHealthCheckNeutrinoNode(
      initNode: NeutrinoNode,
      timeout: FiniteDuration
  ): Future[NeutrinoNode] = {

    // make a custom config, set the inactivity timeout very low
    // so we will disconnect our peer organically
    val str =
      s"""
         |bitcoin-s.node.health-check-interval = ${timeout.toString()}
         |bitcoin-s.node.peer-timeout = ${timeout.toString()}
         |""".stripMargin
    val config =
      ConfigFactory.parseString(str)
    NodeTestUtil.getStartedNodeCustomConfig(initNode, config)
  }
}
