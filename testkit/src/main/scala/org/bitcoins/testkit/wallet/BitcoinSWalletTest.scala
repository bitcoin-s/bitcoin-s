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
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.{WalletDbManagement}
import org.scalatest._

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.db.AppConfig
import com.typesafe.config.ConfigFactory
import org.bitcoins.testkit.BitcoinSAppConfig
import java.nio.file.Files

trait BitcoinSWalletTest
    extends fixture.AsyncFlatSpec
    with BitcoinSFixture
    with BeforeAndAfterAll
    with BitcoinSLogger {
  implicit lazy val actorSystem: ActorSystem = ActorSystem({
    // Akka doesn't like funky characters like '$' (which the
    // compiler often generate)
    val regex = "[a-zA-Z0-9]"
    getClass.getSimpleName.filter(_.toString.matches(regex))
  })

  implicit lazy val ec: ExecutionContext = actorSystem.dispatcher

  protected lazy val chainParams: ChainParams = WalletTestUtil.chainParams

  /** Wallet config with data directory set to user temp directory */
  protected implicit lazy val config: BitcoinSAppConfig = {
    val tmpDir = Files.createTempDirectory("bitcoin-s-")
    val conf = ConfigFactory.parseString(s"bitcoin-s.datadir = $tmpDir")
    BitcoinSAppConfig(conf)
  }

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
    implicit val appConfig: WalletAppConfig = wallet.walletConfig
    WalletDbManagement.dropAll().map(_ => ())
  }

  def createNewWallet(): Future[UnlockedWalletApi] = {
    implicit val walletConf: WalletAppConfig = config.walletConf

    for {
      _ <- WalletDbManagement.createAll()
      wallet <- Wallet.initialize().map {
        case InitializeWalletSuccess(wallet) => wallet
        case err: InitializeWalletError      => fail(err)
      }
    } yield wallet
  }

  def withNewWallet(test: OneArgAsyncTest): FutureOutcome =
    makeDependentFixture(build = createNewWallet _, destroy = destroyWallet)(
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
      createNewWallet _,
      createWalletWithBitcoind,
      (_: UnlockedWalletApi, walletWithBitcoind: WalletWithBitcoind) =>
        walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }
}

object BitcoinSWalletTest extends BitcoinSWalletTest {

  // This method has to be here to have this accessible as an object
  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val msg =
      s"Someone called BitcoinSWalletTest.withFixture. This is not what's supposed to happen!"
    throw new RuntimeException(msg)
  }
}
