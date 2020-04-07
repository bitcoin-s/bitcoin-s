package org.bitcoins.wallet.config

import java.nio.file.{Files, Path}

import com.typesafe.config.Config
import org.bitcoins.core.hd._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.AppConfig
import org.bitcoins.keymanager.{KeyManagerParams, WalletStorage}
import org.bitcoins.wallet.db.WalletDbManagement

import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  * @param directory The data directory of the wallet
  * @param conf Optional sequence of configuration overrides
  */
case class WalletAppConfig(
    private val directory: Path,
    private val conf: Config*)
    extends AppConfig {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList
  override protected[bitcoins] def moduleName: String = "wallet"
  override protected[bitcoins] type ConfigType = WalletAppConfig
  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): WalletAppConfig =
    WalletAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

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

  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    logger.debug(s"Initializing wallet setup")

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    val numMigrations = {
      WalletDbManagement.migrate(this)
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

}

object WalletAppConfig {

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(confs: Config*): WalletAppConfig =
    WalletAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs: _*)
}
