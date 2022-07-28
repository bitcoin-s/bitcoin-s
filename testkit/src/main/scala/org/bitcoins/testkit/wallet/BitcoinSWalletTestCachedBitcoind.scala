package org.bitcoins.testkit.wallet

import com.typesafe.config.Config
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.chain.SyncUtil
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.rpc.{
  BitcoindRpcTestUtil,
  CachedBitcoind,
  CachedBitcoindNewest,
  CachedBitcoindV19
}
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.{
  createWalletWithBitcoind,
  createWalletWithBitcoindCallbacks,
  destroyWallet
}
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.{FutureOutcome, Outcome}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/** Bitcoin-s wallet test trait that uses cached bitcoinds
  * rather than fresh bitcoinds.
  *
  * This should be used by default unless there is a reason your
  * test suite needs fresh bitcoin's for every unit test inside of it
  */
trait BitcoinSWalletTestCachedBitcoind
    extends BitcoinSFixture
    with BaseWalletTest
    with EmbeddedPg { _: CachedBitcoind[_] =>

  /** Creates a funded wallet fixture with bitcoind
    * This is different than [[withFundedWalletAndBitcoind()]]
    * in the sense that it does NOT destroy the given bitcoind.
    * It is the responsibility of the caller of this method to
    * do that, if needed.
    */
  def withFundedWalletAndBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient)(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind[_]] = { () =>
      for {
        walletWithBitcoind <- createWalletWithBitcoindCallbacks(
          bitcoind = bitcoind)
        fundedWallet <- FundWalletUtil.fundWalletWithBitcoind(
          walletWithBitcoind)
        _ <- SyncUtil.syncWalletFullBlocks(wallet = fundedWallet.wallet,
                                           bitcoind = bitcoind)
        _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)
      } yield fundedWallet
    }

    makeDependentFixture[WalletWithBitcoind[_]](
      builder,
      { case walletWithBitcoind: WalletWithBitcoind[_] =>
        destroyWallet(walletWithBitcoind)
      })(test)
  }

  def withNewWalletAndBitcoindCached(
      test: OneArgAsyncTest,
      bitcoind: BitcoindRpcClient)(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind[_]] =
      BitcoinSFixture.composeBuildersAndWrap(
        builder = { () =>
          Future.successful(bitcoind)
        },
        dependentBuilder = { (bitcoind: BitcoindRpcClient) =>
          createWalletWithBitcoind(bitcoind)
        },
        wrap =
          (_: BitcoindRpcClient, walletWithBitcoind: WalletWithBitcoind[_]) =>
            walletWithBitcoind
      )

    makeDependentFixture[WalletWithBitcoind[_]](
      builder,
      { case walletWithBitcoind: WalletWithBitcoind[_] =>
        destroyWallet(walletWithBitcoind)
      })(test)
  }

  def withFundedWalletAndBitcoind(test: OneArgAsyncTest)(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    val bitcoindF = BitcoinSFixture
      .createBitcoindWithFunds(None)

    //create a bitcoind, then pretend that it is cached
    //so we can re-use code in withFundedWalletBitcoindCached
    val resultF = for {
      bitcoind <- bitcoindF
      outcome = withFundedWalletAndBitcoindCached(test = test,
                                                  bitcoind = bitcoind)
      f <- outcome.toFuture
    } yield f

    //since we aren't actually caching the bitcoind, we need
    //to shut it down now
    val stoppedBitcoind: Future[Outcome] = resultF.transformWith({
      case Success(outcome) =>
        stopBitcoind(bitcoindF)
          .map(_ => outcome)
      case Failure(err) =>
        stopBitcoind(bitcoindF)
          .flatMap(_ => Future.failed(err))
    })

    val futureOutcome = new FutureOutcome(stoppedBitcoind)
    futureOutcome
  }

  /** Helper method to stop a Future[BitcoindRpcClient] */
  private def stopBitcoind(
      bitcoindF: Future[BitcoindRpcClient]): Future[Unit] = {
    for {
      bitcoind <- bitcoindF
      _ <- BitcoindRpcTestUtil.stopServer(bitcoind)
    } yield ()
  }

  override def afterAll(): Unit = {
    super[BaseWalletTest].afterAll()
  }
}

trait BitcoinSWalletTestCachedBitcoindNewest
    extends BitcoinSWalletTestCachedBitcoind
    with CachedBitcoindNewest {

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withFundedWalletAndBitcoindCached(test, bitcoind)(
        getFreshWalletAppConfig)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }

  override def afterAll(): Unit = {
    super[CachedBitcoindNewest].afterAll()
    super[BitcoinSWalletTestCachedBitcoind].afterAll()
  }
}

trait BitcoinSWalletTestCachedBitcoinV19
    extends BitcoinSWalletTestCachedBitcoind
    with CachedBitcoindV19 {

  override def afterAll(): Unit = {
    super[CachedBitcoindV19].afterAll()
    super[BitcoinSWalletTestCachedBitcoind].afterAll()
  }

  /** Creates a funded wallet fixture with bitcoind
    * This is different than [[withFundedWalletAndBitcoind()]]
    * in the sense that it does NOT destroy the given bitcoind.
    * It is the responsibility of the caller of this method to
    * do that, if needed.
    */
  def withFundedWalletAndBitcoindCachedV19(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String],
      bitcoind: BitcoindV19RpcClient,
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    val bip39PasswordConfig: Config =
      BitcoinSWalletTest.buildBip39PasswordConfig(bip39PasswordOpt)

    implicit val newWalletConf =
      walletAppConfig.withOverrides(bip39PasswordConfig)
    val builder: () => Future[WalletWithBitcoindV19] = { () =>
      for {
        walletWithBitcoind <- createWalletWithBitcoindCallbacks(
          bitcoind = bitcoind)
        walletWithBitcoindV19 = WalletWithBitcoindV19(
          walletWithBitcoind.wallet,
          bitcoind,
          walletWithBitcoind.walletConfig)
        fundedWallet <- FundWalletUtil
          .fundWalletWithBitcoind(walletWithBitcoindV19)
        _ <- SyncUtil.syncWalletFullBlocks(wallet = fundedWallet.wallet,
                                           bitcoind = bitcoind)
        _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)
      } yield fundedWallet
    }

    makeDependentFixture[WalletWithBitcoind[_]](
      builder,
      destroy = { case walletWithBitcoind: WalletWithBitcoind[_] =>
        destroyWallet(walletWithBitcoind)
      })(test)
  }

  def withNewWalletAndBitcoindCachedV19(
      test: OneArgAsyncTest,
      bitcoind: BitcoindV19RpcClient)(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind[_]] =
      BitcoinSFixture.composeBuildersAndWrap(
        builder = { () =>
          Future.successful(bitcoind)
        },
        dependentBuilder = { (bitcoind: BitcoindV19RpcClient) =>
          BitcoinSWalletTest.createWalletWithBitcoindV19(bitcoind)
        },
        wrap =
          (_: BitcoindRpcClient, walletWithBitcoind: WalletWithBitcoindV19) =>
            walletWithBitcoind
      )

    makeDependentFixture[WalletWithBitcoind[_]](
      builder,
      { case walletWithBitcoind: WalletWithBitcoind[_] =>
        destroyWallet(walletWithBitcoind)
      })(test)
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val f: Future[Outcome] = for {
      bitcoind <- cachedBitcoindWithFundsF
      futOutcome = withFundedWalletAndBitcoindCachedV19(test,
                                                        getBIP39PasswordOpt(),
                                                        bitcoind,
                                                        getFreshWalletAppConfig)
      fut <- futOutcome.toFuture
    } yield fut
    new FutureOutcome(f)
  }
}
