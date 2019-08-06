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
import org.bitcoins.core.currency._
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
  import BitcoinSWalletTest._
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

  def withNewWalletAndBitcoind(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = composeBuildersAndWrap(
      builder = createDefaultWallet _,
      dependentBuilder = createWalletWithBitcoind,
      wrap = (_: UnlockedWalletApi, walletWithBitcoind: WalletWithBitcoind) =>
        walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)

  }

  def withFundedWalletAndBitcoind(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] =
      composeBuildersAndWrapFuture(
        builder = createDefaultWallet _,
        dependentBuilder = createWalletWithBitcoind,
        processResult = (_: UnlockedWalletApi, pair: WalletWithBitcoind) =>
          fundWalletWithBitcoind(pair)
      )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

}

object BitcoinSWalletTest extends BitcoinSLogger {

  case class WalletWithBitcoind(
      wallet: UnlockedWalletApi,
      bitcoind: BitcoindRpcClient)

  /** Returns a function that can be used to create a wallet fixture.
    * If you pass in a configuration to this method that configuration
    * is given to the wallet as user-provided overrides. You could for
    * example use this to override the default data directory, network
    * or account type.
    */
  private def createNewWallet(extraConfig: Option[Config])(
      implicit config: BitcoinSAppConfig,
      ec: ExecutionContext): () => Future[UnlockedWalletApi] =
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
              throw new RuntimeException(
                s"Failed to intialize wallet in fixture with err=${err}")
          }
      }
    }

  /** Creates a wallet with the default configuration  */
  private def createDefaultWallet()(
      implicit config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[UnlockedWalletApi] =
    createNewWallet(None)(config, ec)() // get the standard config

  /** Pairs the given wallet with a bitcoind instance that has money in the bitcoind wallet */
  def createWalletWithBitcoind(wallet: UnlockedWalletApi)(
      implicit system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    val bitcoindF = BitcoinSFixture.createBitcoindWithFunds()
    bitcoindF.map(WalletWithBitcoind(wallet, _))
  }

  /** Creates a default wallet, and then pairs it with a bitcoind instance that has money in the bitcoind wallet */
  def createWalletWithBitcoind()(
      implicit system: ActorSystem,
      config: BitcoinSAppConfig): Future[WalletWithBitcoind] = {
    import system.dispatcher
    val unlockedWalletApiF = createDefaultWallet()
    unlockedWalletApiF.flatMap(u => createWalletWithBitcoind(u))
  }

  /** Gives us a funded bitcoin-s wallet and the bitcoind instance that funded that wallet */
  def fundedWalletAndBitcoind()(
      implicit config: BitcoinSAppConfig,
      system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    for {
      wallet <- createDefaultWallet()
      withBitcoind <- createWalletWithBitcoind(wallet)
      funded <- fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

  /** Funds the given wallet with money from the given bitcoind */
  def fundWalletWithBitcoind(pair: WalletWithBitcoind)(
      implicit ec: ExecutionContext): Future[WalletWithBitcoind] = {
    val WalletWithBitcoind(wallet, bitcoind) = pair
    for {
      addr <- wallet.getNewAddress()
      tx <- bitcoind
        .sendToAddress(addr, 25.bitcoins)
        .flatMap(bitcoind.getRawTransaction(_))

      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      _ <- wallet.processTransaction(tx.hex, 6)
      balance <- wallet.getBalance()

    } yield {
      assert(balance >= 25.bitcoins)
      pair
    }
  }

  def destroyWalletWithBitcoind(walletWithBitcoind: WalletWithBitcoind)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val WalletWithBitcoind(wallet, bitcoind) = walletWithBitcoind
    val stopF = bitcoind.stop()
    val destroyWalletF = destroyWallet(wallet)
    for {
      _ <- stopF
      _ <- destroyWalletF
    } yield ()
  }

  def destroyWallet(wallet: UnlockedWalletApi)(
      implicit ec: ExecutionContext): Future[Unit] = {
    WalletDbManagement
      .dropAll()(config = wallet.walletConfig, ec = ec)
      .map(_ => ())
  }

}
