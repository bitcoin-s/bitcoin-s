package org.bitcoins.testkit.rpc

import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.bitcoins.rpc.client.v18.BitcoindV18RpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
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
      () => BitcoindRpcTestUtil.createNodeTriple(BitcoindVersion.newest),
      destroy = {
        case nodes: (BitcoindRpcClient, BitcoindRpcClient, BitcoindRpcClient) =>
          BitcoindRpcTestUtil.stopServers(Vector(nodes._1, nodes._2, nodes._3))
      }
    )(test)
  }
}

/** Bitcoind fixtures with a cached a bitcoind instance */
trait BitcoindFixturesCached extends BitcoindFixtures {
  _: BitcoinSAsyncFixtureTest with CachedBitcoind[_] =>
}

/** Bitcoind fixtures with a cached a bitcoind instance that is funded */
trait BitcoindFixturesFundedCached extends BitcoindFixtures {
  _: BitcoinSAsyncFixtureTest with CachedBitcoindFunded[_] =>

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withNewestFundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }
}

trait BitcoindFixturesFundedCachedV18
    extends BitcoindFixturesFundedCached
    with CachedBitcoindV18 { _: BitcoinSAsyncFixtureTest =>

  override type FixtureParam = BitcoindV18RpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withV18FundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withV18FundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV18RpcClient): FutureOutcome = {
    makeDependentFixture[BitcoindV18RpcClient](
      () => Future.successful(bitcoind),
      { case _ =>
        Future.unit // don't want to destroy anything since it is cached
      })(test)
  }
}

trait BitcoindFixturesFundedCachedV19
    extends BitcoindFixturesFundedCached
    with CachedBitcoindV19 { _: BitcoinSAsyncFixtureTest =>

  override type FixtureParam = BitcoindV19RpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withV19FundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withV19FundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV19RpcClient): FutureOutcome = {
    makeDependentFixture[BitcoindV19RpcClient](
      () => Future.successful(bitcoind),
      { case _ =>
        Future.unit // don't want to destroy anything since it is cached
      })(test)
  }
}

trait BitcoindFixturesFundedCachedV20
    extends BitcoindFixturesFundedCached
    with CachedBitcoindV20 { _: BitcoinSAsyncFixtureTest =>
  override type FixtureParam = BitcoindV20RpcClient

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
  override type FixtureParam = BitcoindV21RpcClient

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
trait BitcoindFixturesCachedPair[T <: BitcoindRpcClient]
    extends BitcoindFixturesCached
    with CachedBitcoindPair[T] {
  _: BitcoinSAsyncFixtureTest =>

  def with2BitcoindsCached(
      test: OneArgAsyncTest,
      bitcoinds: NodePair[T]): FutureOutcome = {
    makeDependentFixture[NodePair[T]](
      () => Future.successful(bitcoinds),
      destroy = { case _: NodePair[T] =>
        //do nothing since we are caching bitcoinds
        //the test trait may want to re-use them
        Future.unit
      }
    )(test)
  }
}

trait BitcoindFixturesCachedPairV17
    extends BitcoindFixturesCachedPair[BitcoindV17RpcClient] {
  _: BitcoinSAsyncFixtureTest =>
  override type FixtureParam = NodePair[BitcoindV17RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.V17
}

trait BitcoindFixturesCachedPairV18
    extends BitcoindFixturesCachedPair[BitcoindV18RpcClient] {
  _: BitcoinSAsyncFixtureTest =>
  override type FixtureParam = NodePair[BitcoindV18RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.V18
}

trait BitcoindFixturesCachedPairNewest
    extends BitcoindFixturesCachedPair[BitcoindV21RpcClient] {
  _: BitcoinSAsyncFixtureTest =>
  override type FixtureParam = NodePair[BitcoindV21RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.newest
}

/** Bitcoind fixtures with three cached bitcoins that are connected via p2p */
trait BitcoindFixturesCachedTriple[T <: BitcoindRpcClient]
    extends BitcoindFixturesCached
    with CachedBitcoindTriple[T] {
  _: BitcoinSAsyncFixtureTest =>

  def with3BitcoindsCached(
      test: OneArgAsyncTest,
      bitcoinds: NodeTriple[T]): FutureOutcome = {
    makeDependentFixture[NodeTriple[T]](
      () => Future.successful(bitcoinds),
      destroy = { case _: NodeTriple[T] =>
        //do nothing since we are caching bitcoinds
        //the test trait may want to re-use them
        Future.unit
      }
    )(test)
  }
}
