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
import org.bitcoins.testkit.BitcoinSAppConfig
import org.bitcoins.testkit.BitcoinSAppConfig._
import com.typesafe.config.Config

trait BitcoinSWalletTest
    extends fixture.AsyncFlatSpec
    with BitcoinSFixture
    with BeforeAndAfterAll
    with BitcoinSLogger {
  implicit val actorSystem: ActorSystem = ActorSystem(getClass.getSimpleName)
  implicit val ec: ExecutionContext = actorSystem.dispatcher

  protected lazy val chainParams: ChainParams = WalletTestUtil.chainParams

  /** Wallet config with data directory set to user temp directory */
  implicit protected lazy val config: BitcoinSAppConfig =
    BitcoinSAppConfig.getTestConfig()

  /** Timeout for async operations */
  protected val timeout: FiniteDuration = 10.seconds

  protected val networkParam: RegTest.type = WalletTestUtil.networkParam

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(actorSystem)
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.walletConf)
  }

  def destroyWallet(wallet: UnlockedWalletApi): Future[Unit] = {
    WalletDbManagement
      .dropAll()(config = config.walletConf, ec = implicitly[ExecutionContext])
      .map(_ => ())
  }

  private val createNewWallet: Option[Config] => () => Future[
    UnlockedWalletApi] =
    extraConfig =>
      () => {
        val defaultConf = implicitly[WalletAppConfig]
        val walletConfig = extraConfig match {
          case None    => defaultConf
          case Some(c) => defaultConf.withOverrides(c)
        }

        AppConfig.throwIfDefaultDatadir(walletConfig)

        for {
          _ <- walletConfig.initialize()
          wallet <- Wallet
            .initialize()(implicitly[ExecutionContext], walletConfig)
            .map {
              case InitializeWalletSuccess(wallet) => wallet
              case err: InitializeWalletError =>
                logger.error(s"Could not initialize wallet: $err")
                fail(err)
            }
        } yield wallet
    }

  /** Creates a wallet with the default configuration  */
  def withDefaultWallet(): Future[UnlockedWalletApi] =
    createNewWallet(None)() // get the standard config

  /** Lets you customize the parameters for the created wallet */
  val withNewConfiguredWallet: Config => OneArgAsyncTest => FutureOutcome =
    walletConfig =>
      makeDependentFixture(build = createNewWallet(Some(walletConfig)),
                           destroy = destroyWallet)

  def withNewWallet(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(build = withDefaultWallet, destroy = destroyWallet)(
      test)

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
      withDefaultWallet,
      createWalletWithBitcoind,
      (_: UnlockedWalletApi, walletWithBitcoind: WalletWithBitcoind) =>
        walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

}
