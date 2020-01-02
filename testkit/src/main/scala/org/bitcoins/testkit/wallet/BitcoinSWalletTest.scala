package org.bitcoins.testkit.wallet

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.api.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.core.currency._
import org.bitcoins.core.gcs.BlockFilter
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.AppConfig
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.BitcoinSAppConfig._
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.wallet.api.UnlockedWalletApi
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

  def nodeApi: NodeApi = MockNodeApi

  // This is a random block on testnet
  val testBlockHash = DoubleSha256DigestBE.fromHex(
    "00000000496dcc754fabd97f3e2df0a7337eab417d75537fecf97a7ebb0e7c75")

  def chainQueryApi: ChainQueryApi = new ChainQueryApi {

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
      Future.successful({
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
      })
  }

  /** Lets you customize the parameters for the created wallet */
  val withNewConfiguredWallet: Config => OneArgAsyncTest => FutureOutcome = {
    walletConfig =>
      val km = createNewKeyManager()
      makeDependentFixture(
        build = createNewWallet(km, Some(walletConfig), nodeApi, chainQueryApi),
        destroy = destroyWallet)
  }

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
      createDefaultWallet(nodeApi, chainQueryApi)
    }, destroy = destroyWallet)(test)

  def withNewWalletAndBitcoind(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[WalletWithBitcoind] = composeBuildersAndWrap(
      builder = { () =>
        createDefaultWallet(nodeApi, chainQueryApi)
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
          createDefaultWallet(nodeApi, chainQueryApi)
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

  object MockNodeApi extends NodeApi {

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
  }

  case class WalletWithBitcoind(
      wallet: UnlockedWalletApi,
      bitcoind: BitcoindRpcClient)

  private def createNewKeyManager()(
      implicit config: BitcoinSAppConfig): BIP39KeyManager = {
    val keyManagerE = BIP39KeyManager.initialize(config.walletConf.kmParams)
    keyManagerE match {
      case Right(keyManager) => keyManager
      case Left(err) =>
        throw new RuntimeException(s"Cannot initialize key manager err=${err}")
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
      extraConfig: Option[Config],
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(
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
        val wallet =
          Wallet(keyManager, nodeApi, chainQueryApi)(walletConfig, ec)
        Wallet.initialize(wallet)
      }
    }

  /** Creates a wallet with the default configuration  */
  private def createDefaultWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(
      implicit config: BitcoinSAppConfig,
      ec: ExecutionContext): Future[UnlockedWalletApi] = {
    val km = createNewKeyManager()
    createNewWallet(
      keyManager = km,
      extraConfig = None,
      nodeApi = nodeApi,
      chainQueryApi = chainQueryApi)(config, ec)() // get the standard config
  }

  /** Pairs the given wallet with a bitcoind instance that has money in the bitcoind wallet */
  def createWalletWithBitcoind(
      wallet: UnlockedWalletApi
  )(implicit system: ActorSystem): Future[WalletWithBitcoind] = {
    val bitcoindF = BitcoinSFixture.createBitcoindWithFunds()
    bitcoindF.map(WalletWithBitcoind(wallet, _))(system.dispatcher)
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
  ): Future[WalletWithBitcoind] = {
    Future.successful(WalletWithBitcoind(wallet, bitcoindRpcClient))
  }

  /** Gives us a funded bitcoin-s wallet and the bitcoind instance that funded that wallet */
  def fundedWalletAndBitcoind(
      versionOpt: Option[BitcoindVersion],
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(
      implicit config: BitcoinSAppConfig,
      system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    for {
      wallet <- createDefaultWallet(nodeApi, chainQueryApi)
      withBitcoind <- createWalletWithBitcoind(wallet, versionOpt)
      funded <- fundWalletWithBitcoind(withBitcoind)
    } yield funded
  }

  def fundedWalletAndBitcoind(
      bitcoindRpcClient: BitcoindRpcClient,
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi)(
      implicit config: BitcoinSAppConfig,
      system: ActorSystem): Future[WalletWithBitcoind] = {
    import system.dispatcher
    for {
      wallet <- createDefaultWallet(nodeApi, chainQueryApi)
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
      txId <- bitcoind.sendToAddress(addr, initialFunds)
      _ <- bitcoind.getNewAddress.flatMap(bitcoind.generateToAddress(6, _))
      tx <- bitcoind.getRawTransaction(txId)
      _ <- wallet.processTransaction(tx.hex, tx.blockhash)
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
    val destroyWalletF = WalletDbManagement
      .dropAll()(config = wallet.walletConfig, ec = ec)
      .map(_ => ())

    destroyWalletF
  }

}
