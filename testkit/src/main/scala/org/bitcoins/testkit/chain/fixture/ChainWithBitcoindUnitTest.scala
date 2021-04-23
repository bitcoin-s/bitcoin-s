package org.bitcoins.testkit.chain.fixture

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.chain.{ChainDbUnitTest, ChainUnitTest, SyncUtil}
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
      createBitcoindV19ChainHandler(bitcoindV19RpcClient)
    }
    makeDependentFixture(builder, destroyChainApi)(test)
  }

  def createBitcoindV19ChainHandler(
      bitcoindV19RpcClient: BitcoindV19RpcClient): Future[
    BitcoindV19ChainHandler] = {

    val chainApiWithBitcoindF = createChainApiWithBitcoindV19Rpc(
      bitcoindV19RpcClient)

    //now sync the chain api to the bitcoind node
    val syncedBitcoindWithChainHandlerF = for {
      chainApiWithBitcoind <- chainApiWithBitcoindF
      bitcoindWithChainHandler <- SyncUtil.syncBitcoindV19WithChainHandler(
        chainApiWithBitcoind)
    } yield bitcoindWithChainHandler

    syncedBitcoindWithChainHandlerF
  }

  /** Destroys the chain api, but leaves the bitcoind instance running
    * so we can cache it
    */
  def destroyChainApi(bitcoindV19ChainHandler: BitcoindV19ChainHandler)(implicit
      chainAppConfig: ChainAppConfig): Future[Unit] = {
    val _ = bitcoindV19ChainHandler
    ChainUnitTest.destroyAllTables()(chainAppConfig, system.dispatcher)
  }

  private def createChainApiWithBitcoindV19Rpc(
      bitcoind: BitcoindV19RpcClient): Future[BitcoindV19ChainHandler] = {
    val handlerWithGenesisHeaderF =
      ChainUnitTest.setupHeaderTableWithGenesisHeader()

    val chainHandlerF = handlerWithGenesisHeaderF.map(_._1)

    chainHandlerF.map { handler =>
      BitcoindV19ChainHandler(bitcoind, handler)
    }
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindV19].afterAll()
    super[ChainWithBitcoindUnitTest].afterAll()
  }
}
