package org.bitcoins.testkit.chain.fixture

import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.rpc.{CachedBitcoind, CachedBitcoindNewest}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

/** Chain unit test that requires a cached bitcoind type to be injected */
trait ChainWithBitcoindUnitTest extends ChainUnitTest {
  self: CachedBitcoind[?] =>

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
      bitcoindRpcClient: BitcoindRpcClient
  ): FutureOutcome = {
    val builder: () => Future[BitcoindBaseVersionChainHandlerViaRpc] = { () =>
      val c = chainAppConfig
      for {
        _ <- c.start()
        chainApiBitcoind <- ChainUnitTest.createChainApiWithBitcoindRpc(
          bitcoindRpcClient)(using executionContext, c)
      } yield chainApiBitcoind
    }
    val destroy: BitcoindBaseVersionChainHandlerViaRpc => Future[Unit] = {
      case b: BitcoindBaseVersionChainHandlerViaRpc =>
        ChainUnitTest.destroyChainApi()(
          using system,
          b.chainHandler.chainConfig)
    }
    makeDependentFixture(builder, destroy)(test)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[ChainWithBitcoindUnitTest].afterAll()
  }
}
