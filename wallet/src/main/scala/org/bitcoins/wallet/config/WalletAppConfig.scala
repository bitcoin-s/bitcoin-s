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

case class WalletAppConfig(private val conf: Config*) extends AppConfig {
  override val configOverrides: List[Config] = conf.toList
  override def moduleName: String = "wallet"
  override type ConfigType = WalletAppConfig
  override def newConfigOfType(configs: Seq[Config]): WalletAppConfig =
    WalletAppConfig(configs: _*)

  lazy val defaultAccountKind: HDPurpose =
    config.getString("wallet.defaultAccountType") match {
      case "legacy"        => HDPurposes.Legacy
      case "segwit"        => HDPurposes.SegWit
      case "nested-segwit" => HDPurposes.NestedSegWit
      // todo: validate this pre-app startup
      case other: String =>
        throw new RuntimeException(s"$other is not a valid account type!")
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
