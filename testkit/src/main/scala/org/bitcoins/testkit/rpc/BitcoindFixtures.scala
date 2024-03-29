package org.bitcoins.testkit.rpc

import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v21.BitcoindV21RpcClient
import org.bitcoins.rpc.client.v22.BitcoindV22RpcClient
import org.bitcoins.rpc.client.v23.BitcoindV23RpcClient
import org.bitcoins.rpc.client.v24.BitcoindV24RpcClient
import org.bitcoins.rpc.util.{NodePair, NodeTriple}
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.BitcoinSAsyncFixtureTest
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

/** A trait that is useful if you need bitcoind fixtures for your test suite */
trait BitcoindFixtures extends BitcoinSFixture with EmbeddedPg {
  _: BitcoinSAsyncFixtureTest =>

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

/** Test trait that caches a [[BitcoindV21RpcClient]] that is funded
  * and available to use with fixtures
  */
trait BitcoindFixturesFundedCachedV21
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindV21 {
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

  override def afterAll(): Unit = {
    super[CachedBitcoindV21].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Test trait that caches a [[BitcoindV22RpcClient]] that is funded
  * and available to use with fixtures
  */
trait BitcoindFixturesFundedCachedV22
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindV22 {
  override type FixtureParam = BitcoindV22RpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withV22FundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withV22FundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV22RpcClient): FutureOutcome = {
    makeDependentFixture[BitcoindV22RpcClient](
      () => Future.successful(bitcoind),
      { case _ =>
        Future.unit // don't want to destroy anything since it is cached
      })(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindV22].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Test trait that caches a [[BitcoindV23RpcClient]] that is funded
  * and available to use with fixtures
  */
trait BitcoindFixturesFundedCachedV23
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindV23 {
  override type FixtureParam = BitcoindV23RpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withV23FundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withV23FundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV23RpcClient): FutureOutcome = {
    makeDependentFixture[BitcoindV23RpcClient](
      () => Future.successful(bitcoind),
      { case _ =>
        Future.unit // don't want to destroy anything since it is cached
      })(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindV23].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

trait BitcoindFixturesFundedCachedV24
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindV24 {
  override type FixtureParam = BitcoindV24RpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withV24FundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withV24FundedBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV24RpcClient): FutureOutcome = {
    makeDependentFixture[BitcoindV24RpcClient](
      () => Future.successful(bitcoind),
      { _ =>
        Future.unit // don't want to destroy anything since it is cached
      })(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindV24].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

trait BitcoindFixturesFundedCachedNewest
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindNewest {
  override type FixtureParam = BitcoindRpcClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withNewestFundedBitcoindCached(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Bitcoind fixtures with two cached bitcoind that are connected via p2p */
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

trait BitcoindFixturesCachedPairV21
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindV21RpcClient] {
  override type FixtureParam = NodePair[BitcoindV21RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.V21

  override def afterAll(): Unit = {
    super[BitcoindFixturesCachedPair].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Bitcoind fixtures with two cached bitcoind rpc clients that are [[BitcoindVersion.newest]] that are connected via p2p */
trait BitcoindFixturesCachedPairV22
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindV22RpcClient] {
  override type FixtureParam = NodePair[BitcoindV22RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.V22

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val futOutcome = for {
      pair <- clientsF
      futOutcome = with2BitcoindsCached(test, pair)
      f <- futOutcome.toFuture
    } yield f
    new FutureOutcome(futOutcome)
  }

  override def afterAll(): Unit = {
    super[BitcoindFixturesCachedPair].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

trait BitcoindFixturesCachedPairNewest
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindRpcClient] {
  override type FixtureParam = NodePair[BitcoindRpcClient]
  override val version: BitcoindVersion = BitcoindVersion.newest

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val futOutcome = for {
      pair <- clientsF
      futOutcome = with2BitcoindsCached(test, pair)
      f <- futOutcome.toFuture
    } yield f
    new FutureOutcome(futOutcome)
  }

  override def afterAll(): Unit = {
    super[BitcoindFixturesCachedPair].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Bitcoind fixtures with three cached bitcoind that are connected via p2p */
trait BitcoindFixturesCachedTriple[T <: BitcoindRpcClient]
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCached
    with CachedBitcoindTriple[T] {

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

  override def afterAll(): Unit = {
    super[CachedBitcoindTriple].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}
