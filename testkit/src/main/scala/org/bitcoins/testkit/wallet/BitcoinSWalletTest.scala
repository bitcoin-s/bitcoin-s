package org.bitcoins.testkit.wallet

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.bitcoins.core.config.RegTest
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
import org.bitcoins.wallet.db.{WalletDbManagement}
import org.scalatest._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.db.AppConfig
import org.bitcoins.server.BitcoinSAppConfig
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import org.bitcoins.testkit.BitcoinSTestAppConfig

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
    BitcoinSTestAppConfig.getTestConfig()

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
      .dropAll()(config = wallet.walletConfig,
                 ec = implicitly[ExecutionContext])
      .map(_ => ())
  }

  /** Returns a function that can be used to create a wallet fixture.
    * If you pass in a configuration to this method that configuration
    * is given to the wallet as user-provided overrides. You could for
    * example use this to override the default data directory, network
    * or account type.
    */
  private def createNewWallet(
      extraConfig: Option[Config]): () => Future[UnlockedWalletApi] =
    () => {
      val defaultConf = config.walletConf
      val walletConfig = extraConfig match {
        case None    => defaultConf
        case Some(c) => defaultConf.withOverrides(c)
      }

      // we want to check we're not overwriting
      // any user data
      AppConfig.throwIfDefaultDatadir(walletConfig)

      walletConfig.initialize().flatMap { _ =>
        Wallet
          .initialize()(implicitly[ExecutionContext], walletConfig)
          .map {
            case InitializeWalletSuccess(wallet) => wallet
            case err: InitializeWalletError =>
              logger.error(s"Could not initialize wallet: $err")
              fail(err)
          }
      }
    }

  /** Creates a wallet with the default configuration  */
  private def createDefaultWallet(): Future[UnlockedWalletApi] =
    createNewWallet(None)() // get the standard config

  /** Lets you customize the parameters for the created wallet */
  val withNewConfiguredWallet: Config => OneArgAsyncTest => FutureOutcome =
    walletConfig =>
      makeDependentFixture(build = createNewWallet(Some(walletConfig)),
                           destroy = destroyWallet)

  /** Fixture for an initialized wallet which produce legacy addresses */
  def withLegacyWallet(test: OneArgAsyncTest): FutureOutcome = {
    val confOverride =
      ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = legacy")
    withNewConfiguredWallet(confOverride)(test)
  }

  /** Fixture for an initialized wallet which produce segwit addresses */
  def withSegwitWallet(test: OneArgAsyncTest): FutureOutcome = {
    val confOverride =
      ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = segwit")
    withNewConfiguredWallet(confOverride)(test)
  }

  def withNewWallet(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(build = createDefaultWallet _,
                         destroy = destroyWallet)(test)

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
      createDefaultWallet _,
      createWalletWithBitcoind,
      (_: UnlockedWalletApi, walletWithBitcoind: WalletWithBitcoind) =>
        walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

}
