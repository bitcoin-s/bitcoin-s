package org.bitcoins.wallet.config

import com.typesafe.config.Config
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.hd._
import org.bitcoins.core.util.Mutable
import org.bitcoins.core.wallet.keymanagement.{
  KeyManagerInitializeError,
  KeyManagerParams
}
import org.bitcoins.crypto.AesPassword
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.db._
import org.bitcoins.keymanager.bip39.{BIP39KeyManager, BIP39LockedKeyManager}
import org.bitcoins.keymanager.config.KeyManagerAppConfig
import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.AccountDAO
import org.bitcoins.wallet.{Wallet, WalletCallbacks, WalletLogger}

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  * @param directory The data directory of the wallet
  * @param conf Optional sequence of configuration overrides
  */
case class WalletAppConfig(
    private val directory: Path,
    private val conf: Config*)(implicit override val ec: ExecutionContext)
    extends DbAppConfig
    with WalletDbManagement
    with JdbcProfileComponent[WalletAppConfig] {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList

  override protected[bitcoins] def moduleName: String =
    WalletAppConfig.moduleName

  override protected[bitcoins] type ConfigType = WalletAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): WalletAppConfig =
    WalletAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def appConfig: WalletAppConfig = this

  private val callbacks = new Mutable(WalletCallbacks.empty)

  def walletCallbacks: WalletCallbacks = callbacks.atomicGet

  def addCallbacks(newCallbacks: WalletCallbacks): WalletCallbacks = {
    callbacks.atomicUpdate(newCallbacks)(_ + _)
  }

  lazy val kmConf: KeyManagerAppConfig =
    KeyManagerAppConfig(directory, conf: _*)

  lazy val defaultAccountKind: HDPurpose =
    config.getString("bitcoin-s.wallet.defaultAccountType") match {
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
    config.getDouble("bitcoin-s.wallet.bloomFalsePositiveRate")

  lazy val addressGapLimit: Int =
    config.getInt("bitcoin-s.wallet.addressGapLimit")

  lazy val discoveryBatchSize: Int =
    config.getInt("bitcoin-s.wallet.discoveryBatchSize")

  lazy val requiredConfirmations: Int = {
    val confs = config.getInt("bitcoin-s.wallet.requiredConfirmations")
    require(confs >= 1,
            s"requiredConfirmations cannot be less than 1, got: $confs")
    confs
  }

  lazy val feeProviderNameOpt: Option[String] = {
    config.getStringOrNone("bitcoin-s.fee-provider.name")
  }

  lazy val feeProviderTargetOpt: Option[Int] =
    config.getIntOpt("bitcoin-s.fee-provider.target")

  lazy val bip39PasswordOpt: Option[String] = kmConf.bip39PasswordOpt

  lazy val aesPasswordOpt: Option[AesPassword] = kmConf.aesPasswordOpt

  lazy val walletNameOpt: Option[String] = kmConf.walletNameOpt

  override lazy val dbPath: Path = {
    val pathStrOpt =
      config.getStringOrNone(s"bitcoin-s.$moduleName.db.path")
    (pathStrOpt, walletNameOpt) match {
      case (Some(pathStr), Some(walletName)) =>
        Paths.get(pathStr).resolve(walletName)
      case (Some(pathStr), None) =>
        Paths.get(pathStr)
      case (None, Some(_)) | (None, None) =>
        sys.error(s"Could not find dbPath for $moduleName.db.path")
    }
  }

  override lazy val schemaName: Option[String] = {
    (driver, walletNameOpt) match {
      case (PostgreSQL, Some(walletName)) =>
        Some(s"${moduleName}_$walletName")
      case (PostgreSQL, None) =>
        Some(moduleName)
      case (SQLite, None) | (SQLite, Some(_)) =>
        None
    }
  }

  override def start(): Future[Unit] = {
    for {
      _ <- super.start()
    } yield {
      logger.debug(s"Initializing wallet setup")

      if (Files.notExists(datadir)) {
        Files.createDirectories(datadir)
      }

      val numMigrations = {
        migrate()
      }

      logger.info(s"Applied $numMigrations to the wallet project")
    }
  }

  /** The path to our encrypted mnemonic seed */
  private[bitcoins] lazy val seedPath: Path = kmConf.seedPath

  /** Checks if our wallet as a mnemonic seed associated with it */
  def seedExists(): Boolean = kmConf.seedExists()

  def kmParams: KeyManagerParams =
    KeyManagerParams(kmConf.seedPath, defaultAccountKind, network)

  /** How much elements we can have in [[org.bitcoins.wallet.internal.AddressHandling.addressRequestQueue]]
    * before we throw an exception
    */
  def addressQueueSize: Int = {
    if (config.hasPath("bitcoin-s.wallet.addressQueueSize")) {
      config.getInt("bitcoin-s.wallet.addressQueueSize")
    } else {
      100
    }
  }

  /** How long we wait while generating an address in [[org.bitcoins.wallet.internal.AddressHandling.addressRequestQueue]]
    * before we timeout
    */
  def addressQueueTimeout: Duration = {
    if (config.hasPath("bitcoin-s.wallet.addressQueueTimeout")) {
      val javaDuration =
        config.getDuration("bitcoin-s.wallet.addressQueueTimeout")
      new FiniteDuration(javaDuration.toNanos, TimeUnit.NANOSECONDS)
    } else {
      5.second
    }
  }

  /** Checks if the following exist
    *  1. A seed exists
    *  2. wallet exists
    *  3. The account exists
    */
  private def hasWallet()(implicit
      walletConf: WalletAppConfig,
      ec: ExecutionContext): Future[Boolean] = {
    if (kmConf.seedExists()) {
      val hdCoin = walletConf.defaultAccount.coin
      val walletDB = walletConf.dbPath resolve walletConf.dbName
      walletConf.driver match {
        case PostgreSQL =>
          AccountDAO().read((hdCoin, 0)).map(_.isDefined)
        case SQLite =>
          if (Files.exists(walletDB))
            AccountDAO().read((hdCoin, 0)).map(_.isDefined)
          else Future.successful(false)
      }
    } else {
      Future.successful(false)
    }
  }

  /** Creates a wallet based on this [[WalletAppConfig]] */
  def createHDWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit ec: ExecutionContext): Future[Wallet] = {
    WalletAppConfig.createHDWallet(nodeApi = nodeApi,
                                   chainQueryApi = chainQueryApi,
                                   feeRateApi = feeRateApi)(this, ec)
  }

}

object WalletAppConfig
    extends AppConfigFactory[WalletAppConfig]
    with WalletLogger {

  val moduleName: String = "wallet"

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): WalletAppConfig =
    WalletAppConfig(datadir, confs: _*)

  /** Creates a wallet based on the given [[WalletAppConfig]] */
  def createHDWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit
      walletConf: WalletAppConfig,
      ec: ExecutionContext): Future[Wallet] = {
    walletConf.hasWallet().flatMap { walletExists =>
      val aesPasswordOpt = walletConf.aesPasswordOpt
      val bip39PasswordOpt = walletConf.bip39PasswordOpt

      if (walletExists) {
        logger.info(s"Using pre-existing wallet")
        // TODO change me when we implement proper password handling
        BIP39LockedKeyManager.unlock(aesPasswordOpt,
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
        val keyManagerE: Either[KeyManagerInitializeError, BIP39KeyManager] =
          BIP39KeyManager.initialize(aesPasswordOpt = aesPasswordOpt,
                                     kmParams = walletConf.kmParams,
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
