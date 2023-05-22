package org.bitcoins.node.networking

<<<<<<< HEAD
import org.bitcoins.asyncutil.AsyncUtil
=======
>>>>>>> 1581ca161d6 (Remove PeerHandler as it isn't used in src)
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest
}
import org.scalatest.FutureOutcome

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
}
