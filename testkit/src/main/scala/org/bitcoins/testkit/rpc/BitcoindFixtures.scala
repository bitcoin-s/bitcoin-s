package org.bitcoins.testkit.rpc

import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v16.BitcoindV16RpcClient
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

/** Test trait that caches a [[BitcoindV18RpcClient]] that is funded
  * and available to use with fixtures
  */
trait BitcoindFixturesFundedCachedV18
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindV18 {

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

  override def afterAll(): Unit = {
    super[CachedBitcoindV18].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Test trait that caches a [[BitcoindV19RpcClient]] that is funded
  * and available to use with fixtures
  */
trait BitcoindFixturesFundedCachedV19
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindV19 {

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

  override def afterAll(): Unit = {
    super[CachedBitcoindV19].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
  }
}

/** Test trait that caches a [[BitcoindV20RpcClient]] that is funded
  * and available to use with fixtures
  */
trait BitcoindFixturesFundedCachedV20
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesFundedCached
    with CachedBitcoindV20 {
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

  override def afterAll(): Unit = {
    super[CachedBitcoindV20].afterAll()
    super[BitcoinSAsyncFixtureTest].afterAll()
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

/** Bitcoind fixtures with two cached BitcoindV16RpcClient that are connected via p2p */
trait BitcoindFixturesCachedPairV16
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindV16RpcClient] {
  override type FixtureParam = NodePair[BitcoindV16RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.V16

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

/** Bitcoind fixtures with two cached [[BitcoindV17RpcClient]] that are connected via p2p */
trait BitcoindFixturesCachedPairV17
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindV17RpcClient] {
  override type FixtureParam = NodePair[BitcoindV17RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.V17

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

/** Bitcoind fixtures with two cached [[BitcoindV18RpcClient]] that are connected via p2p */
trait BitcoindFixturesCachedPairV18
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindV18RpcClient] {
  override type FixtureParam = NodePair[BitcoindV18RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.V18

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

/** Bitcoind fixtures with two cached bitcoind rpc clients that are [[BitcoindVersion.newest]] that are connected via p2p */
trait BitcoindFixturesCachedPairNewest
    extends BitcoinSAsyncFixtureTest
    with BitcoindFixturesCachedPair[BitcoindV21RpcClient] {
  override type FixtureParam = NodePair[BitcoindV21RpcClient]

  override val version: BitcoindVersion = BitcoindVersion.newest

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
