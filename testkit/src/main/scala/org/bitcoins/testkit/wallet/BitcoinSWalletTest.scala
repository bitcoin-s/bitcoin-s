package org.bitcoins.testkit.wallet

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee._
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.dlc.wallet.{DLCAppConfig, DLCWallet}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.{NodeCallbacks, OnMerkleBlockReceived}
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.util.CallbackUtil
import org.bitcoins.testkit.EmbeddedPg
import org.bitcoins.testkit.chain.SyncUtil
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import org.bitcoins.testkit.wallet.FundWalletUtil.{
  FundedDLCWallet,
  FundedWallet
}
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen._
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletCallbacks, WalletLogger}
import org.scalatest._

import scala.concurrent._
import scala.concurrent.duration._

trait BitcoinSWalletTest
    extends BitcoinSFixture
    with BaseWalletTest
    with EmbeddedPg {
  import BitcoinSWalletTest._

  implicit protected def getFreshDLCAppConfig: DLCAppConfig = {
    getFreshConfig.dlcConf
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(getFreshConfig.walletConf)
    AppConfig.throwIfDefaultDatadir(getFreshConfig.dlcConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    Await.result(getFreshConfig.chainConf.stop(), 1.minute)
    Await.result(getFreshConfig.nodeConf.stop(), 1.minute)
    Await.result(getFreshConfig.walletConf.stop(), 1.minute)
    Await.result(getFreshConfig.dlcConf.stop(), 1.minute)
    super[EmbeddedPg].afterAll()
  }

  def nodeApi: NodeApi = MockNodeApi

  /** Lets you customize the parameters for the created wallet */
  val withNewConfiguredWallet: Config => OneArgAsyncTest => FutureOutcome = {
    walletConfig =>
      implicit val newWalletConf =
        getFreshWalletAppConfig.withOverrides(walletConfig)
      val bip39PasswordOpt = KeyManagerTestUtil.bip39PasswordOpt
      makeDependentFixture(
        build = createNewWallet(bip39PasswordOpt = bip39PasswordOpt,
                                extraConfig = Some(walletConfig),
                                nodeApi = nodeApi,
                                chainQueryApi = chainQueryApi),
        destroy = destroyWallet
      )
  }

  /** Creates a wallet that is funded with some bitcoin, this wallet is NOT
    * peered with a bitcoind so the funds in the wallet are not tied to an
    * underlying blockchain
    */
  def withFundedWallet(test: OneArgAsyncTest, bip39PasswordOpt: Option[String])(
      implicit walletAppConfig: WalletAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        FundWalletUtil.createFundedWallet(nodeApi,
                                          chainQueryApi,
                                          bip39PasswordOpt),
      destroy = { funded: FundedWallet =>
        destroyWallet(funded.wallet)
      }
    )(test)
  }

  def withFundedSegwitWallet(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String])(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        FundWalletUtil.createFundedWallet(nodeApi,
                                          chainQueryApi,
                                          bip39PasswordOpt,
                                          Some(segwitWalletConf)),
      destroy = { funded: FundedWallet =>
        destroyWallet(funded.wallet)
      }
    )(test)
  }

  /** Creates a wallet that is funded with some bitcoin, this wallet is NOT
    * peered with a bitcoind so the funds in the wallet are not tied to an
    * underlying blockchain
    */
  def withFundedDLCWallet(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String])(implicit
      config: BitcoinSAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        FundWalletUtil.createFundedDLCWallet(nodeApi,
                                             chainQueryApi,
                                             bip39PasswordOpt),
      destroy = { funded: FundedDLCWallet =>
        for {
          _ <- destroyDLCWallet(funded.wallet)
        } yield ()
      }
    )(test)
  }

  /** Fixture for an initialized wallet which produce legacy addresses */
  def withLegacyWallet(test: OneArgAsyncTest): FutureOutcome = {
    withNewConfiguredWallet(legacyWalletConf)(test)
  }

  /** Fixture for an initialized wallet which produce segwit addresses */
  def withSegwitWallet(test: OneArgAsyncTest): FutureOutcome = {
    withNewConfiguredWallet(segwitWalletConf)(test)
  }

  /** Fixture for a wallet with default configuration with no funds in it */
  def withNewWallet(test: OneArgAsyncTest, bip39PasswordOpt: Option[String])(
      implicit walletAppConfig: WalletAppConfig): FutureOutcome =
    makeDependentFixture(
      build = { () =>
        createDefaultWallet(nodeApi, chainQueryApi, bip39PasswordOpt)
      },
      destroy = destroyWallet)(test)

  def withNewWallet2Accounts(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String])(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = { () =>
        createWallet2Accounts(nodeApi, chainQueryApi, bip39PasswordOpt)
      },
      destroy = destroyWallet)(test)
  }

  def withNewWalletAndBitcoind(test: OneArgAsyncTest)(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = composeBuildersAndWrap(
      builder = { () =>
        BitcoinSFixture.createBitcoindWithFunds()
      },
      dependentBuilder = { (bitcoind: BitcoindRpcClient) =>
        createWalletWithBitcoind(bitcoind)
      },
      wrap = (_: BitcoindRpcClient, walletWithBitcoind: WalletWithBitcoind) =>
        walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  def withNewWalletAndBitcoindV19(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String])(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = composeBuildersAndWrap(
      builder = { () =>
        BitcoinSFixture
          .createBitcoindWithFunds(Some(BitcoindVersion.V19))
          .map(_.asInstanceOf[BitcoindV19RpcClient])
      },
      dependentBuilder = { (bitcoind: BitcoindV19RpcClient) =>
        createWalletWithBitcoindV19(bitcoind, bip39PasswordOpt)
      },
      wrap =
        (_: BitcoindV19RpcClient, walletWithBitcoind: WalletWithBitcoindV19) =>
          walletWithBitcoind
    )

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  def withFundedWalletAndBitcoindV19(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String])(implicit
      walletAppConfig: WalletAppConfig): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoindV19] = { () =>
      for {
        bitcoind <-
          BitcoinSFixture
            .createBitcoindWithFunds(Some(BitcoindVersion.V19))
            .map(_.asInstanceOf[BitcoindV19RpcClient])
        wallet <- createWalletWithBitcoindCallbacks(bitcoind, bip39PasswordOpt)
        fundedWallet <- FundWalletUtil.fundWalletWithBitcoind(wallet)
        _ <- SyncUtil.syncWalletFullBlocks(wallet = fundedWallet.wallet,
                                           bitcoind = bitcoind)
        _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)
      } yield {
        WalletWithBitcoindV19(fundedWallet.wallet, bitcoind)
      }
    }

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  def withWalletConfig(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletAppConfig] = () => {
      createWalletAppConfig(pgUrl, Vector.empty)
    }

    val destroy: WalletAppConfig => Future[Unit] = walletAppConfig => {
      destroyWalletAppConfig(walletAppConfig)
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }

  def withWalletConfigNotStarted(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletAppConfig] = () => {
      createWalletAppConfigNotStarted(pgUrl, Vector.empty)
    }

    val destroy: WalletAppConfig => Future[Unit] = walletAppConfig => {
      destroyWalletAppConfig(walletAppConfig)
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }
}

object BitcoinSWalletTest extends WalletLogger {

  val defaultAcctAmts = Vector(1.bitcoin, 2.bitcoin, 3.bitcoin)

  val expectedDefaultAmt: CurrencyUnit =
    defaultAcctAmts.fold(CurrencyUnits.zero)(_ + _)

  val account1Amt = Vector(Bitcoins(0.2), Bitcoins(0.3), Bitcoins(0.5))

  val expectedAccount1Amt: CurrencyUnit =
    account1Amt.fold(CurrencyUnits.zero)(_ + _)

  lazy val initialFunds: CurrencyUnit = expectedDefaultAmt + expectedAccount1Amt

  object MockNodeApi extends NodeApi {

    override def broadcastTransactions(
        transactions: Vector[Transaction]): Future[Unit] =
      Future.unit

    override def downloadBlocks(
        blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = Future.unit

  }

  object MockChainQueryApi extends ChainQueryApi {

    override def getBlockHeight(
        blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
      FutureUtil.none

    override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
      Future.successful(DoubleSha256DigestBE.empty)

    override def getNumberOfConfirmations(
        blockHashOpt: DoubleSha256DigestBE): Future[Option[Int]] =
      FutureUtil.none

    override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
      Future.successful(0)

    override def getFilterCount(): Future[Int] = Future.successful(0)

    override def getFiltersBetweenHeights(
        startHeight: Int,
        endHeight: Int): Future[Vector[FilterResponse]] =
      Future.successful(Vector.empty)

    override def epochSecondToBlockHeight(time: Long): Future[Int] =
      Future.successful(0)

    /** calculates the median time passed */
    override def getMedianTimePast(): Future[Long] =
      Future.successful(0L)
  }

  private[bitcoins] class RandomFeeProvider extends FeeRateApi {
    // Useful for tests
    var lastFeeRate: Option[FeeUnit] = None

    override def getFeeRate(): Future[FeeUnit] = {
      val feeRate = FeeUnitGen.feeUnit.sampleSome

      lastFeeRate = Some(feeRate)
      Future.successful(feeRate)
    }
  }

  def createWalletAppConfig(
      pgUrl: () => Option[String],
      configs: Vector[Config])(implicit
      system: ActorSystem): Future[WalletAppConfig] = {
    import system.dispatcher
    val walletAppConfigF = createWalletAppConfigNotStarted(pgUrl, configs)
    for {
      appConfig <- walletAppConfigF
      _ <- appConfig.start()
    } yield appConfig
  }

  def createWalletAppConfigNotStarted(
      pgUrl: () => Option[String],
      configs: Vector[Config])(implicit
      system: ActorSystem): Future[WalletAppConfig] = {
    val baseConf = BaseWalletTest.getFreshWalletAppConfig(pgUrl, configs)
    val walletNameOpt = if (NumberGenerator.bool.sampleSome) {
      Some(StringGenerators.genNonEmptyString.sampleSome)
    } else None

    val walletConf = walletNameOpt match {
      case Some(walletName) =>
        val walletNameOverride = ConfigFactory.parseString(
          s"bitcoin-s.wallet.walletName = $walletName"
        )

        BitcoinSAppConfig(
          baseConf.baseDatadir,
          (walletNameOverride +: baseConf.configOverrides)).walletConf
      case None => baseConf
    }

    Future.successful(walletConf)
  }

  /** Returns a function that can be used to create a wallet fixture.
    * If you pass in a configuration to this method that configuration
    * is given to the wallet as user-provided overrides. You could for
    * example use this to override the default data directory, network
    * or account type.
    */
  private def createNewWallet(
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config],
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(implicit
      config: WalletAppConfig,
      ec: ExecutionContext): () => Future[Wallet] = { () =>
    {
      val walletConfig = extraConfig match {
        case None    => config
        case Some(c) => config.withOverrides(c)
      }

      val walletConfigWithBip39Pw = bip39PasswordOpt match {
        case Some(pw) =>
          val str = s"""bitcoin-s.keymanager.bip39password="$pw""""
          val bip39Config = ConfigFactory.parseString(str)
          walletConfig.withOverrides(bip39Config)
        case None => walletConfig
      }

      // we want to check we're not overwriting
      // any user data
      AppConfig.throwIfDefaultDatadir(walletConfig)

      walletConfigWithBip39Pw.start().flatMap { _ =>
        val wallet =
          Wallet(nodeApi, chainQueryApi, new RandomFeeProvider)(
            walletConfigWithBip39Pw,
            ec)
        Wallet.initialize(wallet, bip39PasswordOpt)
      }
    }
  }

  private def createDLCWallet(
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config],
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(implicit
      config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[DLCWallet] = {

    val walletConfig = extraConfig match {
      case None    => config
      case Some(c) => config.withOverrides(Vector(c))
    }

    val walletConfigWithBip39Pw = bip39PasswordOpt match {
      case Some(pw) =>
        val str = s"""bitcoin-s.keymanager.bip39password="$pw""""
        val bip39Config = ConfigFactory.parseString(str)
        walletConfig.withOverrides(Vector(bip39Config))
      case None => walletConfig
    }

    // we want to check we're not overwriting
    // any user data
    AppConfig.throwIfDefaultDatadir(walletConfigWithBip39Pw.walletConf)

    val initConfs = for {
      _ <- walletConfigWithBip39Pw.walletConf.start()
      _ <- config.dlcConf.start()
    } yield ()

    initConfs.flatMap { _ =>
      val wallet =
        DLCWallet(nodeApi, chainQueryApi, new RandomFeeProvider)(
          walletConfigWithBip39Pw.walletConf,
          config.dlcConf,
          ec)

      Wallet
        .initialize(wallet, bip39PasswordOpt)
        .map(_.asInstanceOf[DLCWallet])
    }
  }

  /** Creates a wallet with the default configuration */
  def createDefaultWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config] = None)(implicit
      config: WalletAppConfig,
      ec: ExecutionContext): Future[Wallet] = {
    val newWalletConf = extraConfig match {
      case None =>
        config
      case Some(walletConf) =>
        config.withOverrides(walletConf)
    }
    createNewWallet(bip39PasswordOpt = bip39PasswordOpt,
                    extraConfig = extraConfig,
                    nodeApi = nodeApi,
                    chainQueryApi = chainQueryApi)(newWalletConf,
                                                   ec
    )() // get the standard config
  }

  /** Creates a default wallet with bitcoind where the [[ChainQueryApi]] fed to the wallet
    * is implemented by bitcoind
    */
  def createWalletWithBitcoindCallbacks(
      bitcoind: BitcoindRpcClient,
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config] = None)(implicit
      config: WalletAppConfig,
      system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    //we need to create a promise so we can inject the wallet with the callback
    //after we have created it into SyncUtil.getNodeApiWalletCallback
    //so we don't lose the internal state of the wallet
    val walletCallbackP = Promise[Wallet]()
    val walletWithBitcoindF = for {
      wallet <- BitcoinSWalletTest.createWallet2Accounts(bitcoind,
                                                         bitcoind,
                                                         bip39PasswordOpt =
                                                           bip39PasswordOpt,
                                                         extraConfig)
      //create the wallet with the appropriate callbacks now that
      //we have them
      walletWithCallback = Wallet(
        nodeApi =
          SyncUtil.getNodeApiWalletCallback(bitcoind, walletCallbackP.future),
        chainQueryApi = bitcoind,
        feeRateApi = new RandomFeeProvider
      )(wallet.walletConfig, wallet.ec)
      //complete the walletCallbackP so we can handle the callbacks when they are
      //called without hanging forever.
      _ = walletCallbackP.success(walletWithCallback)
    } yield WalletWithBitcoindRpc(walletWithCallback, bitcoind)

    walletWithBitcoindF.failed.foreach(err => walletCallbackP.failure(err))

    walletWithBitcoindF
  }

  def createWallet2Accounts(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config] = None)(implicit
      config: WalletAppConfig,
      system: ActorSystem): Future[Wallet] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher

    val defaultWalletF =
      createDefaultWallet(nodeApi, chainQueryApi, bip39PasswordOpt, extraConfig)
    for {
      wallet <- defaultWalletF
      account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
      newAccountWallet <- wallet.createNewAccount(hdAccount = account1,
                                                  kmParams =
                                                    wallet.keyManager.kmParams)
    } yield newAccountWallet

  }

  def createDLCWallet2Accounts(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config] = None)(implicit
      config: BitcoinSAppConfig,
      system: ActorSystem): Future[DLCWallet] = {
    implicit val ec: ExecutionContextExecutor = system.dispatcher
    for {
      wallet <- createDLCWallet(bip39PasswordOpt = bip39PasswordOpt,
                                extraConfig = extraConfig,
                                nodeApi = nodeApi,
                                chainQueryApi = chainQueryApi)
      account1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
      newAccountWallet <- wallet.createNewAccount(hdAccount = account1,
                                                  kmParams =
                                                    wallet.keyManager.kmParams)
    } yield newAccountWallet.asInstanceOf[DLCWallet]
  }

  /** Pairs the given wallet with a bitcoind instance that has money in the bitcoind wallet */
  def createWalletWithBitcoind(
      wallet: Wallet
  )(implicit system: ActorSystem): Future[WalletWithBitcoind] = {
    val bitcoindF = BitcoinSFixture.createBitcoindWithFunds()
    bitcoindF.map(WalletWithBitcoindRpc(wallet, _))(system.dispatcher)
  }

  /** Pairs the given wallet with a bitcoind instance that has money in the bitcoind wallet */
  def createWalletWithBitcoind(
      wallet: Wallet,
      versionOpt: Option[BitcoindVersion]
  )(implicit system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    val bitcoindF = BitcoinSFixture.createBitcoindWithFunds(versionOpt)
    bitcoindF.map(WalletWithBitcoindRpc(wallet, _))
  }

  def createWalletWithBitcoind(bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem,
      config: WalletAppConfig): Future[WalletWithBitcoind] = {
    createWalletWithBitcoindCallbacks(bitcoind, None)
  }

  def createWalletWithBitcoindV19(wallet: Wallet)(implicit
      system: ActorSystem): Future[WalletWithBitcoindV19] = {
    import system.dispatcher
    val createdF =
      createWalletWithBitcoind(wallet, versionOpt = Some(BitcoindVersion.V19))
    for {
      created <- createdF
    } yield WalletWithBitcoindV19(
      created.wallet,
      created.bitcoind.asInstanceOf[BitcoindV19RpcClient])
  }

  def createWalletWithBitcoindV19(
      bitcoind: BitcoindV19RpcClient,
      bip39PasswordOpt: Option[String])(implicit
      system: ActorSystem,
      config: WalletAppConfig): Future[WalletWithBitcoindV19] = {
    import system.dispatcher
    for {
      created <- createWalletWithBitcoindCallbacks(bitcoind, bip39PasswordOpt)
    } yield WalletWithBitcoindV19(created.wallet, bitcoind)
  }

  def createWalletWithBitcoind(
      bitcoind: BitcoindRpcClient,
      bip39PasswordOpt: Option[String])(implicit
      system: ActorSystem,
      config: WalletAppConfig): Future[WalletWithBitcoindRpc] = {
    import system.dispatcher
    for {
      created <- createWalletWithBitcoindCallbacks(bitcoind, bip39PasswordOpt)
    } yield WalletWithBitcoindRpc(created.wallet, bitcoind)
  }

  def createWalletWithBitcoind(
      wallet: Wallet,
      bitcoindRpcClient: BitcoindRpcClient
  ): Future[WalletWithBitcoind] = {
    Future.successful(WalletWithBitcoindRpc(wallet, bitcoindRpcClient))
  }

  /** Gives us a funded bitcoin-s wallet and the bitcoind instance that funded that wallet */
  def fundedWalletAndBitcoind(
      versionOpt: Option[BitcoindVersion],
      nodeApi: NodeApi,
      bip39PasswordOpt: Option[String],
      chainQueryApi: ChainQueryApi,
      walletCallbacks: WalletCallbacks)(implicit
      config: BitcoinSAppConfig,
      system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    config.walletConf.addCallbacks(walletCallbacks)
    for {
      wallet <- BitcoinSWalletTest.createWallet2Accounts(
        nodeApi,
        chainQueryApi,
        bip39PasswordOpt)(config.walletConf, system)
      withBitcoind <- createWalletWithBitcoind(wallet, versionOpt)
      funded <- FundWalletUtil.fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

  /** Funds a wallet with bitcoind, this method adds [[BitcoinSWalletTest.createNodeCallbacksForWallet()]]
    * which processes filters/blocks that can be used to fund the wallet.
    *
    * It's important to note that this does NOT synchronize the wallet with a chain state.
    * This should be done by the caller of this method. A useful method to help you with that
    * in neutrino node cases is [[BitcoinSWalletTest.awaitWalletBalances]]
    */
  def fundedWalletAndBitcoind(
      bitcoindRpcClient: BitcoindRpcClient,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      bip39PasswordOpt: Option[String],
      walletCallbacks: WalletCallbacks)(implicit
      config: BitcoinSAppConfig,
      system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    config.walletConf.addCallbacks(walletCallbacks)
    for {
      wallet <- BitcoinSWalletTest.createWallet2Accounts(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi,
        bip39PasswordOpt = bip39PasswordOpt)(config.walletConf, system)
      //add callbacks for wallet
      nodeCallbacks <-
        BitcoinSWalletTest.createNeutrinoNodeCallbacksForWallet(wallet)(
          config.nodeConf,
          system.dispatcher)
      _ = config.nodeConf.addCallbacks(nodeCallbacks)
      withBitcoind <- createWalletWithBitcoind(wallet, bitcoindRpcClient)
      funded <- FundWalletUtil.fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

  def destroyWalletWithBitcoind(walletWithBitcoind: WalletWithBitcoind)(implicit
      ec: ExecutionContext): Future[Unit] = {
    val (wallet, bitcoind) =
      (walletWithBitcoind.wallet, walletWithBitcoind.bitcoind)
    val stopF = bitcoind.stop()
    val destroyWalletF = destroyWallet(wallet)
    for {
      _ <- stopF
      _ <- destroyWalletF
    } yield ()
  }

  def destroyWallet(wallet: Wallet): Future[Unit] = {
    import wallet.ec
    for {
      _ <- wallet.walletConfig.dropTable("flyway_schema_history")
      _ <- wallet.walletConfig.dropAll()
      _ <- wallet.stop()
      _ <- destroyWalletAppConfig(wallet.walletConfig)
    } yield ()
  }

  def destroyDLCWallet(wallet: DLCWallet): Future[Unit] = {
    import wallet.ec
    for {
      _ <- destroyWallet(wallet)
      _ <- wallet.dlcConfig.stop()
    } yield ()
  }

  def destroyWalletAppConfig(walletAppConfig: WalletAppConfig): Future[Unit] = {
    val stoppedF = walletAppConfig.stop()
    stoppedF
  }

  /** Constructs callbacks for the wallet from the node to process blocks and compact filters */
  def createNeutrinoNodeCallbacksForWallet(wallet: Wallet)(implicit
      nodeAppConfig: NodeAppConfig,
      ec: ExecutionContext): Future[NodeCallbacks] = {
    CallbackUtil.createNeutrinoNodeCallbacksForWallet(wallet)
  }

  /** Registers a callback to handle merkle blocks given to us by a spv node */
  def createSpvNodeCallbacksForWallet(wallet: Wallet)(implicit
      ec: ExecutionContext): NodeCallbacks = {
    val onMerkleBlockReceived: OnMerkleBlockReceived = {
      case (merkleBlock, txs) =>
        for {
          _ <- wallet.processTransactions(txs,
                                          Some(merkleBlock.blockHeader.hashBE))
        } yield ()
    }
    NodeCallbacks(onMerkleBlockReceived = Vector(onMerkleBlockReceived))
  }

  /** Makes sure our wallet is fully funded with the default amounts specified in
    * [[BitcoinSWalletTest]]. This will future won't be completed until balances satisfy [[isSameWalletBalances()]]
    */
  def awaitWalletBalances(fundedWallet: WalletWithBitcoind)(implicit
      config: WalletAppConfig,
      system: ActorSystem): Future[Unit] = {
    AsyncUtil.retryUntilSatisfiedF(conditionF =
                                     () => isSameWalletBalances(fundedWallet),
                                   interval = 1.seconds,
                                   maxTries = 100)(system.dispatcher)
  }

  private def isSameWalletBalances(fundedWallet: WalletWithBitcoind)(implicit
      config: WalletAppConfig,
      system: ActorSystem): Future[Boolean] = {
    import system.dispatcher
    val defaultAccount = config.defaultAccount
    val hdAccount1 = WalletTestUtil.getHdAccount1(config)
    val expectedDefaultAmt = BitcoinSWalletTest.expectedDefaultAmt
    val expectedAccount1Amt = BitcoinSWalletTest.expectedAccount1Amt
    val defaultBalanceF = fundedWallet.wallet.getBalance(defaultAccount)
    val account1BalanceF = fundedWallet.wallet.getBalance(hdAccount1)
    for {
      balance <- defaultBalanceF
      account1Balance <- account1BalanceF
    } yield {
      balance == expectedDefaultAmt &&
      account1Balance == expectedAccount1Amt
    }
  }

}
