package org.bitcoins.node

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeTestWithCachedBitcoindV19
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.bitcoins.testkit.util.TorUtil
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class NeutrinoUnsupportedPeerTest extends NodeTestWithCachedBitcoindV19 {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    if (TorUtil.torEnabled) {
      // We must skip this test for tor enabled
      // because bitcoind only supported tor v2 at the time
      // which is now deprecated and no longer supported by
      // the tor network
      FutureOutcome.succeeded
    } else {
      val outcomeF: Future[Outcome] = for {
        bitcoind <- cachedBitcoindWithFundsF
        outcome = withNeutrinoNodeUnstarted(test, bitcoind)(system,
                                                            getFreshConfig)
        f <- outcome.toFuture
      } yield f
      new FutureOutcome(outcomeF)
    }
  }

  behavior of "NeutrinoNode"

  it must "throw RuntimeException if peer does not support compact filters" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      val exception = recoverToExceptionIf[RuntimeException](node.start())
      exception.map(e =>
        assert(e.getMessage == "No peers supporting compact filters!"))
  }
}
