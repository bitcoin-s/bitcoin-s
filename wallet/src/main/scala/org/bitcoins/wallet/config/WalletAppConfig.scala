package org.bitcoins.wallet.config

import com.typesafe.config.Config
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.commons.config.{AppConfigFactory, ConfigOps}
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.hd._
import org.bitcoins.core.util.Mutable
import org.bitcoins.core.wallet.keymanagement._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.AesPassword
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.db._
import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.db.util.{DBMasterXPubApi, MasterXPubUtil}
import org.bitcoins.keymanager.config.KeyManagerAppConfig
import org.bitcoins.tor.config.TorAppConfig
import org.bitcoins.wallet.config.WalletAppConfig.RebroadcastTransactionsRunnable
import org.bitcoins.wallet.db.WalletDbManagement
import org.bitcoins.wallet.models.AccountDAO
import org.bitcoins.wallet.{Wallet, WalletCallbacks, WalletLogger}

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent._
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  * @param directory The data directory of the wallet
  * @param conf Optional sequence of configuration overrides
  */
case class WalletAppConfig(baseDatadir: Path, configOverrides: Vector[Config])(
    implicit override val ec: ExecutionContext)
    extends DbAppConfig
    with WalletDbManagement
    with JdbcProfileComponent[WalletAppConfig]
    with DBMasterXPubApi {

  override protected[bitcoins] def moduleName: String =
    WalletAppConfig.moduleName

  override protected[bitcoins] type ConfigType = WalletAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): WalletAppConfig =
    WalletAppConfig(baseDatadir, configs.toVector)

  override def appConfig: WalletAppConfig = this

  lazy val torConf: TorAppConfig =
    TorAppConfig(baseDatadir, Some(moduleName), configOverrides)

  private[wallet] lazy val scheduler: ScheduledExecutorService = {
    Executors.newScheduledThreadPool(
      1,
      AsyncUtil.getNewThreadFactory(
        s"bitcoin-s-wallet-scheduler-${System.currentTimeMillis()}"))
  }

  private lazy val rescanThreadFactory: ThreadFactory =
    AsyncUtil.getNewThreadFactory("bitcoin-s-rescan")

  /** Threads for rescanning the wallet */
  private[wallet] lazy val rescanThreadPool: ExecutorService =
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors() * 2,
                                 rescanThreadFactory)

  private val callbacks = new Mutable(WalletCallbacks.empty)

  def walletCallbacks: WalletCallbacks = callbacks.atomicGet

  def addCallbacks(newCallbacks: WalletCallbacks): WalletCallbacks = {
    callbacks.atomicUpdate(newCallbacks)(_ + _)
  }

  lazy val kmConf: KeyManagerAppConfig =
    KeyManagerAppConfig(baseDatadir, configOverrides)

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

  lazy val longTermFeeRate: SatoshisPerVirtualByte = {
    val feeRate = config.getInt("bitcoin-s.wallet.longTermFeeRate")
    require(feeRate >= 0,
            s"longTermFeeRate cannot be less than 0, got: $feeRate")
    SatoshisPerVirtualByte.fromLong(feeRate)
  }

  lazy val rebroadcastFrequency: Duration = {
    if (config.hasPath("bitcoin-s.wallet.rebroadcastFrequency")) {
      val javaDuration =
        config.getDuration("bitcoin-s.wallet.rebroadcastFrequency")
      new FiniteDuration(javaDuration.toNanos, TimeUnit.NANOSECONDS)
    } else {
      4.hours
    }
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
  private val masterXPubDAO: MasterXPubDAO = MasterXPubDAO()(ec, this)

  override def start(): Future[Unit] = {
    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    for {
      _ <- super.start()
      _ <- kmConf.start()
      masterXpub = kmConf.toBip39KeyManager.getRootXPub
      numMigrations = migrate()
      isExists <- seedExists()
      _ <- {
        logger.info(
          s"Starting wallet with xpub=${masterXpub} walletName=${walletNameOpt}")
        if (!isExists) {
          masterXPubDAO
            .create(masterXpub)
            .map(_ => ())
        } else {
          MasterXPubUtil.checkMasterXPub(masterXpub, masterXPubDAO)
        }
      }
    } yield {
      logger.debug(s"Initializing wallet setup")
      if (isHikariLoggingEnabled) {
        //.get is safe because hikari logging is enabled
        startHikariLogger(hikariLoggingInterval.get)
      }
      logger.info(s"Applied $numMigrations to the wallet project")
    }
  }

  override def stop(): Future[Unit] = {
    if (isHikariLoggingEnabled) {
      stopHikariLogger()
    }

    stopRebroadcastTxsScheduler()
    //this eagerly shuts down all scheduled tasks on the scheduler
    //in the future, we should actually cancel all things that are scheduled
    //manually, and then shutdown the scheduler
    scheduler.shutdownNow()
    rescanThreadPool.shutdownNow()
    super.stop()
  }

  /** The path to our encrypted mnemonic seed */
  override lazy val seedPath: Path = kmConf.seedPath

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
  def hasWallet()(implicit
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

  private[this] var rebroadcastTransactionsCancelOpt: Option[
    ScheduledFuture[_]] = None

  /** Starts the wallet's rebroadcast transaction scheduler */
  def startRebroadcastTxsScheduler(wallet: Wallet): Unit = synchronized {
    rebroadcastTransactionsCancelOpt match {
      case Some(_) =>
        //already scheduled, do nothing
        ()
      case None =>
        logger.info(s"Starting wallet rebroadcast task")

        val interval = rebroadcastFrequency.toSeconds
        val initDelay = interval
        val future =
          scheduler.scheduleAtFixedRate(RebroadcastTransactionsRunnable(wallet),
                                        initDelay,
                                        interval,
                                        TimeUnit.SECONDS)
        rebroadcastTransactionsCancelOpt = Some(future)
        ()
    }
  }

  /** Kills the wallet's rebroadcast transaction scheduler */
  def stopRebroadcastTxsScheduler(): Unit = synchronized {
    rebroadcastTransactionsCancelOpt match {
      case Some(cancel) =>
        if (!cancel.isCancelled) {
          logger.info(s"Stopping wallet rebroadcast task")
          cancel.cancel(true)
        } else {
          rebroadcastTransactionsCancelOpt = None
        }
        ()
      case None => ()
    }
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
    WalletAppConfig(datadir, confs)

  /** Creates a wallet based on the given [[WalletAppConfig]] */
  def createHDWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit
      walletConf: WalletAppConfig,
      ec: ExecutionContext): Future[Wallet] = {
    walletConf.hasWallet().flatMap { walletExists =>
      val bip39PasswordOpt = walletConf.bip39PasswordOpt

      if (walletExists) {
        logger.info(s"Using pre-existing wallet")
        val wallet =
          Wallet(nodeApi, chainQueryApi, feeRateApi)
        Future.successful(wallet)
      } else {
        logger.info(s"Creating new wallet")
        val unInitializedWallet =
          Wallet(nodeApi, chainQueryApi, feeRateApi)

        Wallet.initialize(wallet = unInitializedWallet,
                          bip39PasswordOpt = bip39PasswordOpt)
      }
    }
  }

  case class RebroadcastTransactionsRunnable(wallet: Wallet)(implicit
      ec: ExecutionContext)
      extends Runnable {

    override def run(): Unit = {
      val f = for {
        txs <- wallet.getTransactionsToBroadcast

        _ = {
          if (txs.size > 1)
            logger.info(s"Rebroadcasting ${txs.size} transactions")
          else if (txs.size == 1)
            logger.info(s"Rebroadcasting ${txs.size} transaction")
        }

        _ <- wallet.nodeApi.broadcastTransactions(txs)
      } yield ()

      // Make sure broadcasting completes
      // Wrap in try in case of spurious failure
      try {
        Await.result(f, 60.seconds)
      } catch {
        case scala.util.control.NonFatal(exn) =>
          logger.error(s"Failed to broadcast txs", exn)
      }
      ()
    }
  }

}
