package org.bitcoins.wallet.util

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.api.{
  InitializeWalletError,
  InitializeWalletSuccess,
  UnlockedWalletApi
}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.{WalletDbManagement}
import org.scalatest._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.db.AppConfig

trait BitcoinSWalletTest
    extends fixture.AsyncFlatSpec
    with BitcoinSFixture
    with BeforeAndAfterAll
    with BitcoinSLogger {
  implicit val actorSystem: ActorSystem = ActorSystem(getClass.getSimpleName)
  implicit val ec: ExecutionContext = actorSystem.dispatcher

  protected lazy val chainParams: ChainParams = WalletTestUtil.chainParams
  protected implicit lazy val appConfig: WalletAppConfig = WalletAppConfig()

  /** Timeout for async operations */
  protected val timeout: FiniteDuration = 10.seconds

  protected val networkParam: RegTest.type = WalletTestUtil.networkParam

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(actorSystem)
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(appConfig)
  }

  def destroyWallet(wallet: UnlockedWalletApi): Future[Unit] =
    WalletDbManagement.dropAll().map(_ => ())

  def createNewWallet(): Future[UnlockedWalletApi] = {

    for {
      _ <- WalletDbManagement.createAll()
      wallet <- Wallet.initialize().map {
        case InitializeWalletSuccess(wallet) => wallet
        case err: InitializeWalletError      => fail(err)
      }
    } yield wallet
  }

  def withNewWallet(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(build = createNewWallet, destroy = destroyWallet)(test)

  case class WalletWithBitcoind(
      wallet: UnlockedWalletApi,
      bitcoind: BitcoindRpcClient)

  def createWalletWithBitcoind(
      wallet: UnlockedWalletApi): Future[WalletWithBitcoind] = {
    val bitcoindF = createBitcoindWithFunds()
    bitcoindF.map(WalletWithBitcoind(wallet, _))
  }

  def destroyWalletWithBitcoind(
      walletWithBitcoind: WalletWithBitcoind): Future[Unit] = {
    val WalletWithBitcoind(wallet, bitcoind) = walletWithBitcoind
    val stopF = bitcoind.stop()
    val destroyWalletF = destroyWallet(wallet)
    for {
      _ <- stopF
      _ <- destroyWalletF
    } yield ()
  }

  def withNewWalletAndBitcoind(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = composeBuildersAndWrap(
      createNewWallet,
      createWalletWithBitcoind,
      (_: UnlockedWalletApi, walletWithBitcoind: WalletWithBitcoind) =>
        walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

}
