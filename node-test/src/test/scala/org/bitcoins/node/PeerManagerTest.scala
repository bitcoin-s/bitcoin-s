package org.bitcoins.node

import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.node.{
  NodeTestUtil,
  NodeTestWithCachedBitcoindNewest
}
import org.bitcoins.testkit.util.TorUtil
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class PeerManagerTest extends NodeTestWithCachedBitcoindNewest {

  /** Wallet config with data directory set to user temp directory */
  override protected def getFreshConfig: BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(pgUrl)
  }

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val torClientF = if (TorUtil.torEnabled) torF else Future.unit

    val outcomeF: Future[Outcome] = for {
      _ <- torClientF
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeUnstarted(test, bitcoind)(system,
                                                          getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  it must "add the default peer to the peer manager" in {
    nodeConnectedWithBitcoinds =>
      val node = nodeConnectedWithBitcoinds.node
      val bitcoind = nodeConnectedWithBitcoinds.bitcoind
      val peerF = NodeTestUtil.getBitcoindPeer(bitcoind)
      val peerManagerF = for {
        peer <- peerF
        manager = PeerManager(Vector(peer), node)(node.executionContext,
                                                  node.system,
                                                  node.nodeAppConfig)
      } yield manager

      for {
        peerManager <- peerManagerF
        peer <- peerF
        _ <- peerManager.start()
        peerHandlerOpt <- peerManager.getPeerHandler(peer)
        //_ <- peerManager.stop()
      } yield {
        assert(peerHandlerOpt.isDefined)
      }
  }
}
