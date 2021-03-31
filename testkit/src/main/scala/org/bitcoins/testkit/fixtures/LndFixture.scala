package org.bitcoins.testkit.fixtures

import org.bitcoins.lnd.rpc.LndRpcClient
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v21.BitcoindV21RpcClient
import org.bitcoins.testkit.lnd.{LndRpcTestClient, LndRpcTestUtil}
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoinSAsyncFixtureTest
import org.scalatest.FutureOutcome

/** A trait that is useful if you need Lnd fixtures for your test suite */
trait LndFixture extends BitcoinSFixture {
  _: BitcoinSAsyncFixtureTest =>

  override type FixtureParam = LndRpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withLnd(test)
  }

  def withLnd(test: OneArgAsyncTest): FutureOutcome = {
    val bitcoindInstance = BitcoindRpcTestUtil.v21Instance()
    val bitcoind = BitcoindV21RpcClient(bitcoindInstance)
    val client = LndRpcTestClient.fromSbtDownload(Some(bitcoind))

    makeDependentFixture[LndRpcClient](
      () => {
        for {
          _ <- bitcoind.start()
          lnd <- client.start()
        } yield lnd
      },
      { lnd =>
        for {
          _ <- lnd.stop()
          _ <- bitcoind.stop()
        } yield ()
      }
    )(test)
  }
}

/** A trait that is useful if you need Lnd fixtures for your test suite */
trait DualLndFixture extends BitcoinSFixture {
  _: BitcoinSAsyncFixtureTest =>

  override type FixtureParam = (BitcoindRpcClient, LndRpcClient, LndRpcClient)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualLnd(test)
  }

  def withDualLnd(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[FixtureParam](
      () => {
        for {
          bitcoind <- LndRpcTestUtil.startedBitcoindRpcClient()
          _ = println("starting bitcoind")
          _ <- bitcoind.start()
          _ = println("creating lnds")
          lnds <- LndRpcTestUtil.createNodePair(bitcoind)
        } yield (bitcoind, lnds._1, lnds._2)
      },
      { param =>
        val (bitcoind, lndA, lndB) = param
        for {
          _ <- lndA.stop()
          _ <- lndB.stop()
          _ <- bitcoind.stop()
        } yield ()
      }
    )(test)
  }
}
