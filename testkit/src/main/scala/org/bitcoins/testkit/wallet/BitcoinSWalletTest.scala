package org.bitcoins.testkit.wallet

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency._
import org.bitcoins.core.node.NodeApi
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.AppConfig
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.wallet.api.{
  InitializeWalletError,
  InitializeWalletSuccess,
  UnlockedWalletApi
}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.{Wallet, WalletLogger}
import org.scalatest._

import scala.concurrent.{ExecutionContext, Future}

trait BitcoinSWalletTest extends BitcoinSFixture with WalletLogger {
  import BitcoinSWalletTest._

  /** Wallet config with data directory set to user temp directory */
  implicit protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.walletConf)
  }

  def nodeApi: NodeApi = new NodeApi {
    override def downloadBlocks(
        blockHashes: Vector[DoubleSha256Digest]): Future[Unit] =
      FutureUtil.unit
  }

  /** Lets you customize the parameters for the created wallet */
  val withNewConfiguredWallet: Config => OneArgAsyncTest => FutureOutcome =
    walletConfig =>
      makeDependentFixture(build = createNewWallet(Some(walletConfig), nodeApi),
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
    makeDependentFixture(build = { () =>
      createDefaultWallet(nodeApi)
    }, destroy = destroyWallet)(test)

  def withNewWalletAndBitcoind(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = composeBuildersAndWrap(
      builder = { () =>
        createDefaultWallet(nodeApi)
      },
      dependentBuilder = { (wallet: UnlockedWalletApi) =>
        createWalletWithBitcoind(wallet)
      },
      wrap = (_: UnlockedWalletApi, walletWithBitcoind: WalletWithBitcoind) =>
        walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)

  }

  def withFundedWalletAndBitcoind(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] =
      composeBuildersAndWrapFuture(
        builder = { () =>
          createDefaultWallet(nodeApi)
        },
        dependentBuilder = { (wallet: UnlockedWalletApi) =>
          createWalletWithBitcoind(wallet)
        },
        processResult = (_: UnlockedWalletApi, pair: WalletWithBitcoind) =>
          fundWalletWithBitcoind(pair)
      )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  def withWalletConfig(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletAppConfig] = () => {
      val walletConf = config.walletConf
      walletConf.initialize().map(_ => walletConf)
    }

    val destroy: WalletAppConfig => Future[Unit] = walletAppConfig => {
      FileUtil.deleteTmpDir(walletAppConfig.datadir)
      FutureUtil.unit
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }

}

object BitcoinSWalletTest extends WalletLogger {

  lazy val initialFunds = 25.bitcoins

  case class WalletWithBitcoind(
      wallet: UnlockedWalletApi,
      bitcoind: BitcoindRpcClient)

  /** Returns a function that can be used to create a wallet fixture.
    * If you pass in a configuration to this method that configuration
    * is given to the wallet as user-provided overrides. You could for
    * example use this to override the default data directory, network
    * or account type.
    */
  private def createNewWallet(extraConfig: Option[Config], nodeApi: NodeApi)(
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
          .initialize(nodeApi)(implicitly[ExecutionContext], walletConfig)
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
  private def createDefaultWallet(nodeApi: NodeApi)(
      implicit config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[UnlockedWalletApi] =
    createNewWallet(None, nodeApi)(config, ec)() // get the standard config

  /** Pairs the given wallet with a bitcoind instance that has money in the bitcoind wallet */
  def createWalletWithBitcoind(
      wallet: UnlockedWalletApi
  )(implicit system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    val bitcoindF = BitcoinSFixture.createBitcoindWithFunds()
    bitcoindF.map(WalletWithBitcoind(wallet, _))
  }

  /** Pairs the given wallet with a bitcoind instance that has money in the bitcoind wallet */
  def createWalletWithBitcoind(
      wallet: UnlockedWalletApi,
      versionOpt: Option[BitcoindVersion]
  )(implicit system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    val bitcoindF = BitcoinSFixture.createBitcoindWithFunds(versionOpt)
    bitcoindF.map(WalletWithBitcoind(wallet, _))
  }

  def createWalletWithBitcoind(
      wallet: UnlockedWalletApi,
      bitcoindRpcClient: BitcoindRpcClient
  )(implicit system: ActorSystem): Future[WalletWithBitcoind] = {
    Future.successful(WalletWithBitcoind(wallet, bitcoindRpcClient))
  }

  /** Gives us a funded bitcoin-s wallet and the bitcoind instance that funded that wallet */
  def fundedWalletAndBitcoind(
      versionOpt: Option[BitcoindVersion],
      nodeApi: NodeApi)(
      implicit config: BitcoinSAppConfig,
      system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    for {
      wallet <- createDefaultWallet(nodeApi)
      withBitcoind <- createWalletWithBitcoind(wallet, versionOpt)
      funded <- fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

  def fundedWalletAndBitcoind(
      bitcoindRpcClient: BitcoindRpcClient,
      nodeApi: NodeApi)(
      implicit config: BitcoinSAppConfig,
      system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    for {
      wallet <- createDefaultWallet(nodeApi)
      withBitcoind <- createWalletWithBitcoind(wallet, bitcoindRpcClient)
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
        .sendToAddress(addr, initialFunds)
        .flatMap(bitcoind.getRawTransaction(_))

      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      _ <- wallet.processTransaction(tx.hex, 6)
      balance <- wallet.getBalance()

    } yield {
      assert(balance >= initialFunds)
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
