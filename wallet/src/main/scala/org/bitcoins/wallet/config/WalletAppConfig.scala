package org.bitcoins.wallet.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig
import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.wallet.db.WalletDbManagement
import scala.util.Failure
import scala.util.Success
import java.nio.file.Files
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.hd.HDPurposes
import org.bitcoins.core.hd.AddressType
import java.nio.file.Path

/** Configuration for the Bitcoin-S wallet
  * @param directory The data directory of the wallet
  * @param confs Optional sequence of configuration overrides
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

  lazy val bloomFalsePositiveRate: Double =
    config.getDouble("wallet.bloomFalsePositiveRate")

  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    logger.debug(s"Initializing wallet setup")

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    val initF = {
      WalletDbManagement.createAll()(this, ec)
    }
    initF.onComplete {
      case Failure(exception) =>
        logger.error(s"Error on wallet setup: ${exception.getMessage}")
      case Success(_) =>
        logger.debug(s"Initializing wallet setup: done")
    }

    initF
  }

}

object WalletAppConfig {

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(confs: Config*): WalletAppConfig =
    WalletAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs: _*)
}
