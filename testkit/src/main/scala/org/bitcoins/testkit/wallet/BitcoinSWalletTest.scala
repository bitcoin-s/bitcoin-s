package org.bitcoins.testkit.wallet

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.BlockFilter
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{FutureUtil, TimeUtil}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.db.AppConfig
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import org.bitcoins.testkit.chain.SyncUtil
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletCallbacks, WalletLogger}
import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent._

trait BitcoinSWalletTest
    extends BitcoinSFixture
    with WalletLogger
    with EmbeddedPg {
  import BitcoinSWalletTest._

  /** Wallet config with data directory set to user temp directory */
  implicit protected def config: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvWithEmbeddedDbTestConfig(pgUrl)

  implicit protected def walletAppConfig: WalletAppConfig = {
    config.walletConf
  }

  override def beforeAll(): Unit = {
    AppConfig.throwIfDefaultDatadir(config.walletConf)
    super[EmbeddedPg].beforeAll()
  }

  override def afterAll(): Unit = {
    Await.result(config.chainConf.stop(), 1.minute)
    Await.result(config.nodeConf.stop(), 1.minute)
    Await.result(config.walletConf.stop(), 1.minute)
    super[EmbeddedPg].afterAll()
  }

  def nodeApi: NodeApi = MockNodeApi

  val legacyWalletConf: Config =
    ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = legacy")

  val segwitWalletConf: Config =
    ConfigFactory.parseString("bitcoin-s.wallet.defaultAccountType = segwit")

  // This is a random block on testnet
  val testBlockHash = DoubleSha256DigestBE.fromHex(
    "00000000496dcc754fabd97f3e2df0a7337eab417d75537fecf97a7ebb0e7c75")

  def chainQueryApi: ChainQueryApi =
    new ChainQueryApi {

      /** Gets the height of the given block */
      override def getBlockHeight(
          blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
        if (blockHash == testBlockHash)
          Future.successful(Some(1))
        else FutureUtil.none

      /** Gets the hash of the block that is what we consider "best" */
      override def getBestBlockHash(): Future[DoubleSha256DigestBE] =
        Future.successful(testBlockHash)

      /** Gets number of confirmations for the given block hash */
      override def getNumberOfConfirmations(
          blockHash: DoubleSha256DigestBE): Future[Option[Int]] =
        if (blockHash == testBlockHash)
          Future.successful(Some(6))
        else FutureUtil.none

      /** Gets the number of compact filters in the database */
      override def getFilterCount: Future[Int] = Future.successful(1)

      /** Returns the block height of the given block stamp */
      override def getHeightByBlockStamp(blockStamp: BlockStamp): Future[Int] =
        Future.successful(1)

      override def getFiltersBetweenHeights(
          startHeight: Int,
          endHeight: Int): Future[Vector[FilterResponse]] =
        Future.successful {
          import scodec.bits._

          // This is a filter for the random block on testnet
          val filterBytes: ByteVector =
            hex"fd2701f0ed169ad16107a8a74609b9e4de3c6133c564f79923ca228805d3" ++
              hex"8e3efc796c4b35034cb573b10b759cdda5efd19e1cdb4d343afcb06455fa" ++
              hex"820b06eca828ad61d3377fa464f3bd06ff4432310a363f667e13d09ba993" ++
              hex"264c703a0aa668b33eaa555bd3e93ac85dfde380ab723aafd407dfa13ffe" ++
              hex"2e7ddf6f452bd0d977617c4ab2dc3b38c26810023984ad57890e3cf34cfc" ++
              hex"2d4a6973b9430ede26bfd9f5bb24e043d48483d84b9025d0a940b15f13fc" ++
              hex"0a1e77abd7626869f417c7710e9a6315477691d7c4e2c50f0e776755a62a" ++
              hex"b6f0e8eb7a3be8d1a8c3d9dd4602efc5146f0d431d1669378d7afa03c7b9" ++
              hex"84d9b0b78007abb6e7c036156e5186d1d79a2f37daecfcbe8821cf42851c" ++
              hex"b10ef0c359307d54e53078eb631f02c067a474dceb484da20bc0e7c5451a" ++
              hex"b957f46b306caa82938b19bb34fd76c5cc07e048932524704dec8f72c91c" ++
              hex"d5ee1f4648de839047a0bea0d4d4d66c19cfccc2b5f285a84af18114f608" ++
              hex"f144391648aedfb5ffcccbb51272512d6ba9a2e19a47cebe5b50a8a7073a" ++
              hex"1c24059440444047a41bdbab16f61bc4b0ee8987de82fd25cc62abc86e2b" ++
              hex"577fc55175be138680df7253a8bcae9d9954391d3bed806ce5a6869b4553" ++
              hex"0f214486b1b7f0347efcfde58ca0882f059f7b1541c74506930897c78e23" ++
              hex"a6c94b49856369606ed652b8c7402a49f289cb5d1098bb999112225327e0" ++
              hex"a32efd2bcd192a2ffbd1997c6a3b7d1a9445bc31fb57485ebe0c431e482b" ++
              hex"04e509e557cff107cee08a45c22aa3cbdcb9d305bd95c919e90239e0ec29" ++
              hex"2a5418a6151f431e8ab82278b3d816ecd483f43d3d657dae9996cc523fdd" ++
              hex"242c4e01935db91a2936e9398ff7278b8a3430eed99ad25fc2a41afc0b4a" ++
              hex"e417f6c1785414607cfa13f04173740333a5b58655c74a51deddb38cf8c3" ++
              hex"d50b7d2ccf380cad34a5c341e7155494cc4560dff3b19bf88b4d73e9ce76" ++
              hex"cbeff573fe93674e4a752d06d5321ff00a4582d62683fb4986d36eaec825" ++
              hex"c14d41b2d5aefaf539e989f7fa097eac657c70b975c56e26b73fb9401ce3" ++
              hex"81502f0883d52c6a3bcc956e0ea1787f0717d0205fecfe55b01edb1ac0"
          Vector(
            FilterResponse(compactFilter = BlockFilter
                             .fromBytes(filterBytes, testBlockHash.flip),
                           blockHash = testBlockHash,
                           blockHeight = 1))
        }

      override def epochSecondToBlockHeight(time: Long): Future[Int] =
        Future.successful(0)
    }

  /** Lets you customize the parameters for the created wallet */
  val withNewConfiguredWallet: Config => OneArgAsyncTest => FutureOutcome = {
    walletConfig =>
      val newWalletConf = walletAppConfig.withOverrides(walletConfig)
      val km = createNewKeyManager()(newWalletConf)
      val bip39PasswordOpt = KeyManagerTestUtil.bip39PasswordOpt
      makeDependentFixture(
        build = createNewWallet(keyManager = km,
                                bip39PasswordOpt = bip39PasswordOpt,
                                extraConfig = Some(walletConfig),
                                nodeApi = nodeApi,
                                chainQueryApi = chainQueryApi),
        destroy = destroyWallet
      )
  }

  /** Creates a wallet that is funded with some bitcoin, this wallet is NOT
    * peered with a bitcoind so the funds in the wallet are not tied to an
    * underlying blockchain */
  def withFundedWallet(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String]): FutureOutcome = {
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
      bip39PasswordOpt: Option[String]): FutureOutcome = {
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

  /** Fixture for an initialized wallet which produce legacy addresses */
  def withLegacyWallet(test: OneArgAsyncTest): FutureOutcome = {
    withNewConfiguredWallet(legacyWalletConf)(test)
  }

  /** Fixture for an initialized wallet which produce segwit addresses */
  def withSegwitWallet(test: OneArgAsyncTest): FutureOutcome = {
    withNewConfiguredWallet(segwitWalletConf)(test)
  }

  def withNewWallet(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String]): FutureOutcome =
    makeDependentFixture(
      build = { () =>
        createDefaultWallet(nodeApi, chainQueryApi, bip39PasswordOpt)
      },
      destroy = destroyWallet)(test)

  def withNewWallet2Accounts(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String]): FutureOutcome = {
    makeDependentFixture(
      build = { () =>
        createWallet2Accounts(nodeApi, chainQueryApi, bip39PasswordOpt)
      },
      destroy = destroyWallet)(test)
  }

  def withNewWalletAndBitcoind(test: OneArgAsyncTest): FutureOutcome = {
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
      bip39PasswordOpt: Option[String]): FutureOutcome = {
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

  def withFundedWalletAndBitcoind(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String]): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = { () =>
      for {
        bitcoind <-
          BitcoinSFixture
            .createBitcoindWithFunds(None)
        wallet <- createWalletWithBitcoindCallbacks(bitcoind = bitcoind,
                                                    bip39PasswordOpt =
                                                      bip39PasswordOpt)
        fundedWallet <- fundWalletWithBitcoind(wallet)
      } yield fundedWallet
    }

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  def withFundedWalletAndBitcoindV19(
      test: OneArgAsyncTest,
      bip39PasswordOpt: Option[String]): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoindV19] = { () =>
      for {
        bitcoind <-
          BitcoinSFixture
            .createBitcoindWithFunds(Some(BitcoindVersion.V19))
            .map(_.asInstanceOf[BitcoindV19RpcClient])
        wallet <- createWalletWithBitcoindCallbacks(bitcoind, bip39PasswordOpt)
        fundedWallet <- fundWalletWithBitcoind(wallet)
      } yield {
        WalletWithBitcoindV19(fundedWallet.wallet, bitcoind)
      }
    }

    makeDependentFixture(builder, destroy = destroyWalletWithBitcoind)(test)
  }

  def withWalletConfig(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletAppConfig] = () => {
      val walletConf = config.walletConf
      walletConf.initialize().map(_ => walletConf)
    }

    val destroy: WalletAppConfig => Future[Unit] = walletAppConfig => {
      FileUtil.deleteTmpDir(walletAppConfig.datadir)
      walletAppConfig.stop()
      FutureUtil.unit
    }
    makeDependentFixture(builder, destroy = destroy)(test)
  }

  def getBIP39PasswordOpt(): Option[String] =
    KeyManagerTestUtil.bip39PasswordOpt
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

    override def broadcastTransaction(transaction: Transaction): Future[Unit] =
      FutureUtil.unit

    override def downloadBlocks(
        blockHashes: Vector[DoubleSha256Digest]): Future[Unit] = FutureUtil.unit

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

    override def getFilterCount: Future[Int] = Future.successful(0)

    override def getFiltersBetweenHeights(
        startHeight: Int,
        endHeight: Int): Future[Vector[FilterResponse]] =
      Future.successful(Vector.empty)

    override def epochSecondToBlockHeight(time: Long): Future[Int] =
      Future.successful(0)
  }

  private def createNewKeyManager(
      bip39PasswordOpt: Option[String] = KeyManagerTestUtil.bip39PasswordOpt)(
      implicit config: WalletAppConfig): BIP39KeyManager = {
    val keyManagerE = BIP39KeyManager.initialize(kmParams = config.kmParams,
                                                 bip39PasswordOpt =
                                                   bip39PasswordOpt)
    keyManagerE match {
      case Right(keyManager) => keyManager
      case Left(err) =>
        throw new RuntimeException(s"Cannot initialize key manager err=${err}")
    }
  }

  private[testkit] class RandomFeeProvider extends FeeRateApi {
    private val rnd = new scala.util.Random(TimeUtil.now.toEpochMilli)
    private val start = 2
    private val end = 100

    def getFeeRate: Future[FeeUnit] = {
      val satoshis = Satoshis(start + rnd.nextInt((end - start) + 1))
      Future.successful(SatoshisPerVirtualByte(satoshis))
    }
  }

  /** Returns a function that can be used to create a wallet fixture.
    * If you pass in a configuration to this method that configuration
    * is given to the wallet as user-provided overrides. You could for
    * example use this to override the default data directory, network
    * or account type.
    */
  private def createNewWallet(
      keyManager: BIP39KeyManager,
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config],
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(implicit
      config: BitcoinSAppConfig,
      ec: ExecutionContext): () => Future[Wallet] =
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
        val wallet =
          Wallet(keyManager,
                 nodeApi,
                 chainQueryApi,
                 new RandomFeeProvider,
                 keyManager.creationTime)(walletConfig, ec)
        Wallet.initialize(wallet, bip39PasswordOpt)
      }
    }

  /** Creates a wallet with the default configuration */
  def createDefaultWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config] = None)(implicit
      config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[Wallet] = {
    val newWalletConf = extraConfig match {
      case None =>
        config.walletConf
      case Some(walletConf) =>
        config.walletConf.withOverrides(walletConf)
    }
    val km =
      createNewKeyManager(bip39PasswordOpt = bip39PasswordOpt)(newWalletConf)
    createNewWallet(
      keyManager = km,
      bip39PasswordOpt = bip39PasswordOpt,
      extraConfig = extraConfig,
      nodeApi = nodeApi,
      chainQueryApi = chainQueryApi)(config, ec)() // get the standard config
  }

  /** Creates a default wallet with bitcoind where the [[ChainQueryApi]] fed to the wallet
    * is implemented by bitcoind */
  def createWalletWithBitcoindCallbacks(
      bitcoind: BitcoindRpcClient,
      bip39PasswordOpt: Option[String],
      extraConfig: Option[Config] = None)(implicit
      config: BitcoinSAppConfig,
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
      _ = wallet.stopWalletThread()

      //create the wallet with the appropriate callbacks now that
      //we have them
      walletWithCallback = Wallet(
        keyManager = wallet.keyManager,
        nodeApi =
          SyncUtil.getNodeApiWalletCallback(bitcoind, walletCallbackP.future),
        chainQueryApi = SyncUtil.getTestChainQueryApi(bitcoind),
        feeRateApi = bitcoind,
        creationTime = wallet.keyManager.creationTime
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
      config: BitcoinSAppConfig,
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
      config: BitcoinSAppConfig): Future[WalletWithBitcoind] = {
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
      config: BitcoinSAppConfig): Future[WalletWithBitcoindV19] = {
    import system.dispatcher
    for {
      created <- createWalletWithBitcoindCallbacks(bitcoind, bip39PasswordOpt)

    } yield WalletWithBitcoindV19(created.wallet, bitcoind)
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
      wallet <- BitcoinSWalletTest.createWallet2Accounts(nodeApi,
                                                         chainQueryApi,
                                                         bip39PasswordOpt)
      withBitcoind <- createWalletWithBitcoind(wallet, versionOpt)
      funded <- fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

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
        bip39PasswordOpt = bip39PasswordOpt)
      withBitcoind <- createWalletWithBitcoind(wallet, bitcoindRpcClient)
      funded <- fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

  /** Funds the given wallet with money from the given bitcoind */
  def fundWalletWithBitcoind[T <: WalletWithBitcoind](pair: T)(implicit
      ec: ExecutionContext): Future[T] = {
    val (wallet, bitcoind) = (pair.wallet, pair.bitcoind)

    val defaultAccount = wallet.walletConfig.defaultAccount
    val fundedDefaultAccountWalletF =
      FundWalletUtil.fundAccountForWalletWithBitcoind(
        amts = defaultAcctAmts,
        account = defaultAccount,
        wallet = wallet,
        bitcoind = bitcoind
      )

    val hdAccount1 = WalletTestUtil.getHdAccount1(wallet.walletConfig)
    val fundedAccount1WalletF = for {
      fundedDefaultAcct <- fundedDefaultAccountWalletF
      fundedAcct1 <- FundWalletUtil.fundAccountForWalletWithBitcoind(
        amts = account1Amt,
        account = hdAccount1,
        wallet = fundedDefaultAcct,
        bitcoind = bitcoind
      )
    } yield fundedAcct1

    //sanity check to make sure we have money
    for {
      fundedWallet <- fundedAccount1WalletF
      balance <- fundedWallet.getBalance(defaultAccount)
      _ = require(
        balance == expectedDefaultAmt,
        s"Funding wallet fixture failed to fund the wallet, got balance=$balance expected=$expectedDefaultAmt")

      account1Balance <- fundedWallet.getBalance(hdAccount1)
      _ = require(
        account1Balance == expectedAccount1Amt,
        s"Funding wallet fixture failed to fund account 1, " +
          s"got balance=$hdAccount1 expected=$expectedAccount1Amt"
      )

    } yield pair
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
    import wallet.walletConfig.ec
    for {

      _ <- wallet.walletConfig.dropTable("flyway_schema_history")
      _ <- wallet.walletConfig.dropAll()
      _ <- wallet.stop()
    } yield ()
  }

}
