package org.bitcoins.testkit.chain.fixture

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.V19BlockFilterRpc
import org.bitcoins.testkit.chain.{ChainDbUnitTest, ChainUnitTest}
import org.bitcoins.testkit.rpc.{
  CachedBitcoind,
  CachedBitcoindBlockFilterRpcNewest,
  CachedBitcoindNewest
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

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[ChainWithBitcoindUnitTest].afterAll()
  }
}

/** Chain Unit test suite that has a cached bitcoind v19 instance */
trait ChainWithBitcoindBlockFilterRpcCachedUnitTest
    extends ChainWithBitcoindUnitTest
    with CachedBitcoindBlockFilterRpcNewest {

  override type FixtureParam = BitcoindBlockFilterRpcChainHandler

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withBitcoindBlockFilterRpcChainHandlerViaRpc(test, bitcoind)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  def withBitcoindBlockFilterRpcChainHandlerViaRpc(
      test: OneArgAsyncTest,
      bitcoindRpcClient: BitcoindRpcClient
        with V19BlockFilterRpc): FutureOutcome = {
    val builder: () => Future[BitcoindBlockFilterRpcChainHandler] = { () =>
      ChainUnitTest.createBitcoindBlockFilterRpcChainHandler(bitcoindRpcClient)
    }

    val destroy: BitcoindBlockFilterRpcChainHandler => Future[Unit] = {
      case _: BitcoindBlockFilterRpcChainHandler =>
        ChainUnitTest.destroyChainApi()
    }
    makeDependentFixture(builder, destroy)(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindBlockFilterRpcNewest].afterAll()
    super[ChainWithBitcoindUnitTest].afterAll()
  }
}
