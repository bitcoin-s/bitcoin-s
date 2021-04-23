package org.bitcoins.testkit.chain.fixture

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.chain.{ChainDbUnitTest, ChainUnitTest}
import org.bitcoins.testkit.rpc.{
  CachedBitcoind,
  CachedBitcoindNewest,
  CachedBitcoindV19
}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

/** Chain unit test that requires a cached bitcoind type to be injected */
trait ChainWithBitcoindUnitTest extends ChainDbUnitTest {
  _: CachedBitcoind[_] =>

}

trait ChainWithBitcoindNewestCachedUnitTest
    extends ChainWithBitcoindUnitTest
    with CachedBitcoindNewest {

  override type FixtureParam = BitcoindBaseVersionChainHandlerViaRpc

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withBitcoindNewestChainHandlerViaRpc(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withBitcoindNewestChainHandlerViaRpc(
      test: OneArgAsyncTest,
      bitcoindRpcClient: BitcoindRpcClient): FutureOutcome = {
    val builder: () => Future[BitcoindBaseVersionChainHandlerViaRpc] = { () =>
      ChainUnitTest.createChainApiWithBitcoindRpc(bitcoindRpcClient)
    }
    val destroy: BitcoindBaseVersionChainHandlerViaRpc => Future[Unit] = {
      case _: BitcoindBaseVersionChainHandlerViaRpc =>
        ChainUnitTest.destroyChainApi()
    }
    makeDependentFixture(builder, destroy)(test)
  }

}

/** Chain Unit test suite that has a cached bitcoind v19 instance */
trait ChainWithBitcoindV19CachedUnitTest
    extends ChainWithBitcoindUnitTest
    with CachedBitcoindV19 {

  override type FixtureParam = BitcoindV19ChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withBitcoindV19ChainHandlerViaRpc(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withBitcoindV19ChainHandlerViaRpc(
      test: OneArgAsyncTest,
      bitcoindV19RpcClient: BitcoindV19RpcClient): FutureOutcome = {
    val builder: () => Future[BitcoindV19ChainHandler] = { () =>
      ChainUnitTest.createBitcoindV19ChainHandler(bitcoindV19RpcClient)
    }

    val destroy: BitcoindV19ChainHandler => Future[Unit] = {
      case _: BitcoindV19ChainHandler =>
        ChainUnitTest.destroyChainApi()
    }
    makeDependentFixture(builder, destroy)(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindV19].afterAll()
    super[ChainWithBitcoindUnitTest].afterAll()
  }
}
