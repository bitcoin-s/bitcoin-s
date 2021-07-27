package org.bitcoins.node

import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.node.NodeTestWithCachedBitcoindV19
import org.bitcoins.testkit.node.fixture.NeutrinoNodeConnectedWithBitcoind
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

class NeutrinoUnsupportedPeerTest extends NodeTestWithCachedBitcoindV19 {

  override protected def getFreshConfig: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoWithEmbeddedDbTestConfig(pgUrl)

  override type FixtureParam = NeutrinoNodeConnectedWithBitcoind

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val outcomeF: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      outcome = withNeutrinoNodeNotSyncedWithBitcoind(test, bitcoind)(
        system,
        getFreshConfig)
      f <- outcome.toFuture
    } yield f
    new FutureOutcome(outcomeF)
  }

  behavior of "NeutrinoNode"

  it must "throw RuntimeException if peer does not support compact filters" in {
    nodeConnectedWithBitcoind: NeutrinoNodeConnectedWithBitcoind =>
      val node = nodeConnectedWithBitcoind.node
      the[RuntimeException] thrownBy node
        .sync() should have message "No peers supporting compact filters!"
  }
}
