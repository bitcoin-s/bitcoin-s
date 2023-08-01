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
import scala.concurrent.duration.DurationInt

class ReConnectionTest extends NodeTestWithCachedBitcoindNewest {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl,
                                                              Vector.empty)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome <- withNeutrinoNodeUnstarted(test, bitcoind)(
        system,
        getFreshConfig).toFuture
    } yield outcome

    new FutureOutcome(f)
  }

  behavior of "ReConnectionTest"

  it must "attempt to reconnect if max connections are full" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      val bitcoind = nodeConnectedWithBitcoind.bitcoind
      val node = nodeConnectedWithBitcoind.node

      val connectedF = for {
        _ <- node.start()
        //wait until we are fully connected before continuing test
        _ <- AsyncUtil.retryUntilSatisfiedF(conditionF = () =>
          node.getConnectionCount.map(_ == 1))
        nodeUri <- NodeTestUtil.getNodeURIFromBitcoind(bitcoind)
        peer <- NodeTestUtil.getBitcoindPeer(bitcoind)
        _ <- bitcoind.disconnectNode(nodeUri)
        _ <- AsyncUtil.retryUntilSatisfiedF(() =>
          node.peerManager.isDisconnected(peer))
        //make sure we re-connect
        _ <- AsyncUtil.retryUntilSatisfiedF(conditionF = () =>
          node.peerManager.isConnected(peer))
      } yield succeed

      connectedF
  }

  it must "disconnect a peer after a period of inactivity" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      val startedF =
        getSmallInactivityCheckNeutrinoNode(nodeConnectedWithBitcoind.node)
      for {
        started <- startedF
        _ <- AsyncUtil.retryUntilSatisfiedF(() =>
          started.getConnectionCount.map(_ == 1))
        //wait until there is a timeout for inactivity
        _ <- AsyncUtil.retryUntilSatisfiedF(
          () => started.getConnectionCount.map(_ == 0),
          1.second)
        _ <- started.stop()
        _ <- started.nodeConfig.stop()
      } yield {
        succeed
      }
  }

  it must "reconnect a peer when inactivity checks run and we have 0 peers" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/5162
      val bitcoind = nodeConnectedWithBitcoind.bitcoind
      val startedF =
        getSmallInactivityCheckNeutrinoNode(nodeConnectedWithBitcoind.node)
      for {
        started <- startedF
        _ <- AsyncUtil.retryUntilSatisfiedF(() =>
          started.getConnectionCount.map(_ == 1))
        //explicitly disconnect it
        bitcoindPeer <- NodeTestUtil.getBitcoindPeer(bitcoind)
        _ <- started.peerManager.disconnectPeer(bitcoindPeer)
        //wait until we have zero connections
        _ <- AsyncUtil.retryUntilSatisfiedF(
          () => started.getConnectionCount.map(_ == 0),
          1.second)

        //wait until there is a timeout for inactivity
        _ <- AsyncUtil.retryUntilSatisfiedF(
          () => started.getConnectionCount.map(_ == 1),
          1.second)
        _ <- started.stop()
        _ <- started.nodeConfig.stop()
      } yield {
        succeed
      }
  }

  private def getSmallInactivityCheckNeutrinoNode(
      initNode: NeutrinoNode): Future[NeutrinoNode] = {

    //make a custom config, set the inactivity timeout very low
    //so we will disconnect our peer organically
    val config =
      ConfigFactory.parseString("bitcoin-s.node.inactivity-timeout=5s")
    val stoppedConfigF = initNode.nodeConfig.stop()
    val newNodeAppConfigF =
      stoppedConfigF.map(_ => initNode.nodeConfig.withOverrides(config))
    val nodeF = {
      for {
        newNodeAppConfig <- newNodeAppConfigF
        _ <- newNodeAppConfig.start()
      } yield {
        NeutrinoNode(
          walletCreationTimeOpt = initNode.walletCreationTimeOpt,
          nodeConfig = newNodeAppConfig,
          chainConfig = initNode.chainAppConfig,
          actorSystem = initNode.system,
          paramPeers = initNode.paramPeers
        )
      }
    }

    val startedF = nodeF.flatMap(_.start())
    startedF
  }
}
