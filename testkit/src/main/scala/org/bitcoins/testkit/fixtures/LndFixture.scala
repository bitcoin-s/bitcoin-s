package org.bitcoins.testkit.fixtures

import org.bitcoins.lnd.rpc.LndRpcClient
import org.bitcoins.lnd.rpc.config.LndInstanceRemote
import org.bitcoins.rpc.client.v23.BitcoindV23RpcClient
import org.bitcoins.testkit.lnd._
import org.bitcoins.testkit.rpc._
import org.scalatest.FutureOutcome

import scala.io.Source

/** A trait that is useful if you need Lnd fixtures for your test suite */
trait LndFixture extends BitcoinSFixture with CachedBitcoindV21 {

  override type FixtureParam = LndRpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withLnd(test)
  }

  def withLnd(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[LndRpcClient](
      () => {
        for {
          bitcoind <- cachedBitcoindWithFundsF

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
trait DualLndFixture extends BitcoinSFixture with CachedBitcoindV23 {

  override type FixtureParam =
    (BitcoindV23RpcClient, LndRpcClient, LndRpcClient)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualLnd(test)
  }

  def withDualLnd(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[FixtureParam](
      () => {
        for {
          bitcoind <- cachedBitcoindWithFundsF
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

trait RemoteLndFixture extends BitcoinSFixture with CachedBitcoindV21 {

  override type FixtureParam = LndRpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withLnd(test)
  }

  def withLnd(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[LndRpcClient](
      () => {
        for {
          bitcoind <- cachedBitcoindWithFundsF

          // start and initialize a lnd
          client = LndRpcTestClient.fromSbtDownload(Some(bitcoind))
          lnd <- client.start()

          // get certificate as a string
          cert = {
            val file = lnd.instance.certFileOpt.get
            val source = Source.fromFile(file)
            val str = source.getLines().toVector.mkString("\n")
            source.close()

            str
          }

          // create a remote instance and client
          remoteInstance = LndInstanceRemote(lnd.instance.rpcUri,
                                             lnd.instance.macaroon,
                                             cert)
          remoteLnd = LndRpcClient(remoteInstance)
        } yield remoteLnd
      },
      { lnd =>
        for {
          _ <- lnd.stop()
        } yield ()
      }
    )(test)
  }
}
