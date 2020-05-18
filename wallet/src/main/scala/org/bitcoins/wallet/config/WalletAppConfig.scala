package org.bitcoins.wallet.config

import java.nio.file.{Files, Path}
import java.util.concurrent.TimeUnit

import com.typesafe.config.Config
import org.bitcoins.core.hd._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.{AppConfig, JdbcProfileComponent}
import org.bitcoins.keymanager.{KeyManagerParams, WalletStorage}
import org.bitcoins.wallet.db.WalletDbManagement

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
}

object WalletAppConfig {

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(useLogbackConf: Boolean, confs: Config*)(
      implicit ec: ExecutionContext): WalletAppConfig =
    WalletAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR,
                    useLogbackConf,
                    confs: _*)
}
