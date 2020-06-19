package org.bitcoins.wallet.config

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import com.typesafe.config.Config
import org.bitcoins.core.api.{ChainQueryApi, FeeRateApi, NodeApi}
import org.bitcoins.core.hd._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.{AppConfig, AppConfigFactory, JdbcProfileComponent}
import org.bitcoins.keymanager.bip39.{BIP39KeyManager, BIP39LockedKeyManager}
import org.bitcoins.keymanager.{
  KeyManagerInitializeError,
  KeyManagerParams,
  WalletStorage
}
import org.bitcoins.wallet.{Wallet, WalletLogger}
import org.bitcoins.wallet.api.WalletApi
import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.AccountDAO

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  * @param directory The data directory of the wallet
  * @param conf Optional sequence of configuration overrides
  */
case class WalletAppConfig(
    private val directory: Path,
    override val useLogbackConf: Boolean,
    private val conf: Config*)(implicit override val ec: ExecutionContext)
    extends AppConfig
    with WalletDbManagement
    with JdbcProfileComponent[WalletAppConfig] {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList
  override protected[bitcoins] def moduleName: String = "wallet"
  override protected[bitcoins] type ConfigType = WalletAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): WalletAppConfig =
    WalletAppConfig(directory, useLogbackConf, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def appConfig: WalletAppConfig = this

  lazy val defaultAccountKind: HDPurpose =
    config.getString("wallet.defaultAccountType") match {
      case "legacy"        => HDPurposes.Legacy
      case "segwit"        => HDPurposes.SegWit
      case "nested-segwit" => HDPurposes.NestedSegWit
      // todo: validate this pre-app startup
      case other: String =>
        throw new RuntimeException(s"$other is not a valid account type!")
    }

  lazy val defaultAddressType: AddressType = {
    defaultAccountKind match {
      case HDPurposes.Legacy       => AddressType.Legacy
      case HDPurposes.NestedSegWit => AddressType.NestedSegWit
      case HDPurposes.SegWit       => AddressType.SegWit
      // todo: validate this pre-app startup
      case other =>
        throw new RuntimeException(s"$other is not a valid account type!")
    }
  }

  lazy val defaultAccount: HDAccount = {
    val purpose = defaultAccountKind
    HDAccount(coin = HDCoin(purpose, HDCoinType.fromNetwork(network)),
              index = 0)
  }

  lazy val bloomFalsePositiveRate: Double =
    config.getDouble("wallet.bloomFalsePositiveRate")

  lazy val addressGapLimit: Int = config.getInt("wallet.addressGapLimit")

  lazy val discoveryBatchSize: Int = config.getInt("wallet.discoveryBatchSize")

  lazy val requiredConfirmations: Int =
    config.getInt("wallet.requiredConfirmations")

  require(
    requiredConfirmations >= 1,
    s"requiredConfirmations cannot be less than 1, got: $requiredConfirmations")

  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    logger.debug(s"Initializing wallet setup")

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    val numMigrations = {
      migrate()
    }

    logger.info(s"Applied $numMigrations to the wallet project")

    FutureUtil.unit
  }

  /** The path to our encrypted mnemonic seed */
  private[bitcoins] def seedPath: Path = {
    baseDatadir.resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
  }

  /** Checks if our wallet as a mnemonic seed associated with it */
  def seedExists(): Boolean = {
    Files.exists(seedPath)
  }

  def kmParams: KeyManagerParams =
    KeyManagerParams(seedPath, defaultAccountKind, network)

  /** How much elements we can have in [[org.bitcoins.wallet.internal.AddressHandling.addressRequestQueue]]
    * before we throw an exception */
  def addressQueueSize: Int = {
    if (config.hasPath("wallet.addressQueueSize")) {
      config.getInt("wallet.addressQueueSize")
    } else {
      100
    }
  }

  /** How long we wait while generating an address in [[org.bitcoins.wallet.internal.AddressHandling.addressRequestQueue]]
    * before we timeout */
  def addressQueueTimeout: scala.concurrent.duration.Duration = {
    if (config.hasPath("wallet.addressQueueTimeout")) {
      val javaDuration = config.getDuration("wallet.addressQueueTimeout")
      new FiniteDuration(javaDuration.toNanos, TimeUnit.NANOSECONDS)
    } else {
      5.second
    }
  }

  /** Checks if the following exist
    *  1. A wallet exists
    *  2. seed exists
    *  3. The account exists */
  def hasWallet()(implicit ec: ExecutionContext): Future[Boolean] = {
    val walletDB = dbPath.resolve(dbName)
    val hdCoin = defaultAccount.coin
    if (Files.exists(walletDB) && seedExists()) {
      AccountDAO()(ec, this).read((hdCoin, 0)).map(_.isDefined)
    } else {
      Future.successful(false)
    }
  }

  /** Creates a wallet based on this [[WalletAppConfig]] */
  def createWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi,
      bip39PasswordOpt: Option[String])(implicit
      ec: ExecutionContext): Future[WalletApi] = {
    WalletAppConfig.createWallet(nodeApi = nodeApi,
                                 chainQueryApi = chainQueryApi,
                                 feeRateApi = feeRateApi,
                                 bip39PasswordOpt = bip39PasswordOpt)(this, ec)
  }
}

object WalletAppConfig
    extends AppConfigFactory[WalletAppConfig]
    with WalletLogger {

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(
      datadir: Path,
      useLogbackConf: Boolean,
      confs: Vector[Config])(implicit ec: ExecutionContext): WalletAppConfig =
    WalletAppConfig(datadir, useLogbackConf, confs: _*)

  /** Creates a wallet based on the given [[WalletAppConfig]] */
  def createWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi,
      bip39PasswordOpt: Option[String])(implicit
      walletConf: WalletAppConfig,
      ec: ExecutionContext): Future[WalletApi] = {
    walletConf.hasWallet().flatMap { walletExists =>
      if (walletExists) {
        logger.info(s"Using pre-existing wallet")
        // TODO change me when we implement proper password handling
        BIP39LockedKeyManager.unlock(BIP39KeyManager.badPassphrase,
                                     bip39PasswordOpt,
                                     walletConf.kmParams) match {
          case Right(km) =>
            val wallet =
              Wallet(km, nodeApi, chainQueryApi, feeRateApi, km.creationTime)
            Future.successful(wallet)
          case Left(err) =>
            sys.error(s"Error initializing key manager, err=${err}")
        }
      } else {
        logger.info(s"Initializing key manager")
        val bip39PasswordOpt = None
        val keyManagerE: Either[KeyManagerInitializeError, BIP39KeyManager] =
          BIP39KeyManager.initialize(kmParams = walletConf.kmParams,
                                     bip39PasswordOpt = bip39PasswordOpt)

        val keyManager = keyManagerE match {
          case Right(keyManager) => keyManager
          case Left(err) =>
            sys.error(s"Error initializing key manager, err=${err}")
        }

        logger.info(s"Creating new wallet")
        val unInitializedWallet =
          Wallet(keyManager,
                 nodeApi,
                 chainQueryApi,
                 feeRateApi,
                 keyManager.creationTime)

        Wallet.initialize(wallet = unInitializedWallet,
                          bip39PasswordOpt = bip39PasswordOpt)
      }
    }
  }
}
