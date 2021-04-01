package org.bitcoins.testkit.fixtures

import org.bitcoins.lnd.rpc.LndRpcClient
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.lnd.{LndRpcTestClient, LndRpcTestUtil}
import org.bitcoins.testkit.rpc.{CachedBitcoindFunded, CachedBitcoindV21}
import org.bitcoins.testkit.util.BitcoinSAsyncFixtureTest
import org.scalatest.FutureOutcome

/** A trait that is useful if you need Lnd fixtures for your test suite */
trait LndFixture extends BitcoinSFixture with CachedBitcoindV21 {
  _: BitcoinSAsyncFixtureTest with CachedBitcoindFunded[_] =>

  override type FixtureParam = LndRpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withLnd(test)
  }

  def withLnd(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[LndRpcClient](
      () => {
        for {
          bitcoind <- cachedBitcoindWithFundsF
          _ <- bitcoind.start()

          client = LndRpcTestClient.fromSbtDownload(Some(bitcoind))
          lnd <- client.start()
        } yield lnd
      },
      { lnd =>
        for {
          _ <- lnd.stop()
        } yield ()
      }
    )(test)
  }
}

/** A trait that is useful if you need Lnd fixtures for your test suite */
trait DualLndFixture extends BitcoinSFixture with CachedBitcoindV21 {
  _: BitcoinSAsyncFixtureTest with CachedBitcoindFunded[_] =>

  override type FixtureParam = (BitcoindRpcClient, LndRpcClient, LndRpcClient)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualLnd(test)
  }

  def withDualLnd(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[FixtureParam](
      () => {
        for {
          bitcoind <- cachedBitcoindWithFundsF
          _ = logger.debug("starting bitcoind")
          _ <- bitcoind.start()
          _ = logger.debug("creating lnds")
          lnds <- LndRpcTestUtil.createNodePair(bitcoind)
        } yield (bitcoind, lnds._1, lnds._2)
      },
      { param =>
        val (_, lndA, lndB) = param
        for {
          _ <- lndA.stop()
          _ <- lndB.stop()
        } yield ()
      }
    )(test)
  }
}
