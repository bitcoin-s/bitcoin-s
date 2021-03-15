package org.bitcoins.testkit.rpc

import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v20.BitcoindV20RpcClient
import org.bitcoins.rpc.client.v21.BitcoindV21RpcClient
import org.bitcoins.rpc.util.{NodePair, NodeTriple}
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

  def with3Bitcoinds(test: OneArgAsyncTest): FutureOutcome = {
    makeDependentFixture[(
        BitcoindRpcClient,
        BitcoindRpcClient,
        BitcoindRpcClient)](
      () => BitcoindRpcTestUtil.createNodeTriple(),
      destroy = {
        case nodes: (BitcoindRpcClient, BitcoindRpcClient, BitcoindRpcClient) =>
          BitcoindRpcTestUtil.stopServers(Vector(nodes._1, nodes._2, nodes._3))
      }
    )(test)
  }
}

/** Bitcoind fixtures with a cached a bitcoind instance */
trait BitcoindFixturesCached extends BitcoindFixtures {
  _: BitcoinSAsyncFixtureTest with CachedBitcoind =>
}

/** Bitcoind fixtures with a cached a bitcoind instance that is funded */
trait BitcoindFixturesFundedCached extends BitcoindFixtures {
  _: BitcoinSAsyncFixtureTest with CachedBitcoindFunded =>

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withNewestFundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }
}

trait BitcoindFixturesFundedCachedV20
    extends BitcoindFixturesFundedCached
    with CachedBitcoindV20 { _: BitcoinSAsyncFixtureTest =>

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withV20FundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withV20FundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV20RpcClient): FutureOutcome = {
    makeDependentFixture[BitcoindV20RpcClient](
      () => Future.successful(bitcoind),
      { case _ =>
        Future.unit // don't want to destroy anything since it is cached
      })(test)
  }
}

trait BitcoindFixturesFundedCachedV21
    extends BitcoindFixturesFundedCached
    with CachedBitcoindV21 { _: BitcoinSAsyncFixtureTest =>

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withV21FundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withV21FundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV21RpcClient): FutureOutcome = {
    makeDependentFixture[BitcoindV21RpcClient](
      () => Future.successful(bitcoind),
      { case _ =>
        Future.unit // don't want to destroy anything since it is cached
      })(test)
  }
}

/** Bitcoind fixtures with three cached bitcoins that are connected via p2p */
trait BitcoindFixturesCachedPair
    extends BitcoindFixturesCached
    with CachedBitcoindPair {
  _: BitcoinSAsyncFixtureTest =>

  def with2BitcoindsCached(
      test: OneArgAsyncTest,
      bitcoinds: NodePair): FutureOutcome = {
    makeDependentFixture[NodePair](
      () => Future.successful(bitcoinds),
      destroy = { case _: NodePair =>
        //do nothing since we are caching bitcoinds
        //the test trait may want to re-use them
        Future.unit
      }
    )(test)
  }
}

/** Bitcoind fixtures with three cached bitcoins that are connected via p2p */
trait BitcoindFixturesCachedTriple
    extends BitcoindFixturesCached
    with CachedBitcoindTriple {
  _: BitcoinSAsyncFixtureTest =>

  def with3BitcoindsCached(
      test: OneArgAsyncTest,
      bitcoinds: NodeTriple): FutureOutcome = {
    makeDependentFixture[NodeTriple](
      () => Future.successful(bitcoinds),
      destroy = { case _: NodeTriple =>
        //do nothing since we are caching bitcoinds
        //the test trait may want to re-use them
        Future.unit
      }
    )(test)
  }
}
