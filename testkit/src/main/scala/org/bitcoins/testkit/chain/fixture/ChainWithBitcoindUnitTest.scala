package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.chain.{ChainDbUnitTest, ChainUnitTest}
import org.bitcoins.testkit.rpc.{CachedBitcoind, CachedBitcoindV19}
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future

/** Chain unit test that requires a cached bitcoind type to be injected */
trait ChainWithBitcoindUnitTest extends ChainDbUnitTest {
  _: CachedBitcoind[_] =>

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
    makeDependentFixture(builder, destroyChainApi)(test)
  }

  /** Destroys the chain api, but leaves the bitcoind instance running
    * so we can cache it
    */
  def destroyChainApi(bitcoindV19ChainHandler: BitcoindV19ChainHandler)(implicit
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    val _ = bitcoindV19ChainHandler
    ChainUnitTest.destroyAllTables()(chainAppConfig, system.dispatcher)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindV19].afterAll()
    super[ChainWithBitcoindUnitTest].afterAll()
  }
}
