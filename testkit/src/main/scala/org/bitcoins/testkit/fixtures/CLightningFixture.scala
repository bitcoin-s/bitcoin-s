package org.bitcoins.testkit.fixtures

import akka.actor.ActorSystem
import com.bitcoins.clightning.rpc.CLightningRpcClient
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.testkit.clightning.{
  CLightningRpcTestClient,
  CLightningRpcTestUtil
}
import org.bitcoins.testkit.rpc._
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome

import scala.concurrent.duration.DurationInt

/** A trait that is useful if you need clightning fixtures for your test suite */
trait CLightningFixture extends BitcoinSFixture with CachedBitcoindV21 {

  override type FixtureParam = CLightningRpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[CLightningRpcClient](
      () =>
        for {
          bitcoind <- cachedBitcoindWithFundsF

          client = CLightningRpcTestClient.fromSbtDownload(Some(bitcoind))
          clightning <- client.start()
        } yield clightning,
      { clightning =>
        for {
          // let clightning clean up
          _ <- TestAsyncUtil.nonBlockingSleep(3.seconds)
          _ <- clightning.stop()
        } yield ()
      }
    )(test)
  }
}

/** A trait that is useful if you need dual clightning fixtures for your test suite */
trait DualCLightningFixture extends BitcoinSFixture with CachedBitcoindV21 {

  override type FixtureParam =
    (BitcoindRpcClient, CLightningRpcClient, CLightningRpcClient)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualCLightning(test)
  }

  def withDualCLightning(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[FixtureParam](
      () => {
        for {
          bitcoind <- cachedBitcoindWithFundsF
          _ = logger.debug("creating clightning")
          clients <- CLightningRpcTestUtil.createNodePair(bitcoind)
        } yield (bitcoind, clients._1, clients._2)
      },
      { param =>
        val (_, clientA, clientB) = param
        for {
          // let clightning clean up
          _ <- TestAsyncUtil.nonBlockingSleep(3.seconds)
          _ <- clientA.stop()
          _ <- clientB.stop()
        } yield ()
      }
    )(test)
  }
}

/** Creates two clightnings with no channels opened */
trait CLightningChannelOpenerFixture
    extends BitcoinSFixture
    with CachedBitcoindV21 {

  override type FixtureParam =
    (BitcoindRpcClient, CLightningRpcClient, CLightningRpcClient)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[FixtureParam](
      () => {
        cachedBitcoindWithFundsF.flatMap { bitcoind =>
          val actorSystemA =
            ActorSystem.create(
              "bitcoin-s-clightning-test-" + FileUtil.randomDirName)
          val clientA = CLightningRpcTestClient
            .fromSbtDownload(Some(bitcoind))(actorSystemA)

          val actorSystemB =
            ActorSystem.create(
              "bitcoin-s-clightning-test-" + FileUtil.randomDirName)
          val clientB = CLightningRpcTestClient
            .fromSbtDownload(Some(bitcoind))(actorSystemB)

          val startAF = clientA.start()
          val startBF = clientB.start()

          for {
            a <- startAF
            b <- startBF

            // wait for rpc servers to start
            _ <- TestAsyncUtil.awaitCondition(() => a.instance.rpcFile.exists(),
                                              interval = 1.second,
                                              maxTries = 500)
            _ <- TestAsyncUtil.awaitCondition(() => b.instance.rpcFile.exists(),
                                              interval = 1.second,
                                              maxTries = 500)
            _ <- TestAsyncUtil.nonBlockingSleep(7.seconds)
          } yield (bitcoind, a, b)
        }
      },
      { param =>
        val (_, clientA, clientB) = param
        for {
          // let clightning clean up
          _ <- TestAsyncUtil.nonBlockingSleep(3.seconds)
          _ <- clientA.stop()
          _ <- clientB.stop()
        } yield ()
      }
    )(test)
  }
}
