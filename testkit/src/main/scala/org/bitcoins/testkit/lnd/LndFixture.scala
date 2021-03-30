package org.bitcoins.testkit.lnd

import org.bitcoins.lnd.rpc.LndRpcClient
import org.bitcoins.rpc.client.v21.BitcoindV21RpcClient
import org.bitcoins.testkit.fixtures.BitcoinSFixture
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
