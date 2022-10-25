package org.bitcoins.node.networking

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.node.networking.peer.PeerHandler
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest,
  NodeUnitTest
}
import org.bitcoins.testkit.util.AkkaUtil
import org.scalatest.FutureOutcome

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class ReConnectionTest extends NodeTestWithCachedBitcoindNewest {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

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
      val peerF = NodeTestUtil.getBitcoindPeer(bitcoind)
      val peerHandlerF: Future[PeerHandler] = peerF.flatMap { peer =>
        NodeUnitTest.buildPeerHandler(peer, None)(node.nodeConfig,
                                                  node.chainConfig,
                                                  system)
      }

      val connectedF = for {
        _ <- node.start()
        peerHandler <- peerHandlerF
        _ = peerHandler.peerMsgSender.connect()
        _ <- AsyncUtil.retryUntilSatisfiedF(() =>
          peerHandler.p2pClient.isInitialized())
        nodeUri <- NodeTestUtil.getNodeURIFromBitcoind(bitcoind)
        _ <- bitcoind.disconnectNode(nodeUri)
        _ <- AkkaUtil.nonBlockingSleep(2.seconds) //time to ensure disconnection
        //now we should eventually automatically reconnect
        _ <- AsyncUtil.retryUntilSatisfiedF(
          conditionF = () => peerHandler.p2pClient.isConnected(),
          interval = 500.millis,
          maxTries = 60)
      } yield succeed

      connectedF
  }
}
