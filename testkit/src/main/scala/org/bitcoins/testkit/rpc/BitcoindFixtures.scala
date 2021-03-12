package org.bitcoins.testkit.rpc

import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.BitcoinSAsyncFixtureTest

import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

trait BitcoindFixtures extends BitcoinSFixture with EmbeddedPg {
  _: BitcoinSAsyncFixtureTest =>

  def withNewestFundedBitcoind(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[BitcoindRpcClient](
      () =>
        BitcoinSFixture.createBitcoindWithFunds(Some(BitcoindVersion.newest)),
      { case bitcoind: BitcoindRpcClient =>
        BitcoindRpcTestUtil.stopServer(bitcoind)
      }
    )(test)
  }

  def withNewestFundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient): FutureOutcome = {
    makeDependentFixture[BitcoindRpcClient](
      () => Future.successful(bitcoind),
      { case _ =>
        Future.unit // don't want to destroy anything since it is cached
      })(test)
  }
}

trait BitcoindFixturesCached extends BitcoindFixtures with CachedBitcoind {
  _: BitcoinSAsyncFixtureTest =>

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withNewestFundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }
}
