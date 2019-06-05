package org.bitcoins.wallet.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig
import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.wallet.db.WalletDbManagement
import scala.util.Failure
import scala.util.Success
import java.nio.file.Files

case class WalletAppConfig(conf: Config*) extends AppConfig {
  override val configOverrides: List[Config] = conf.toList
  override def moduleName: String = "wallet"
  override type ConfigType = WalletAppConfig
  override def newConfigOfType(configs: List[Config]): WalletAppConfig =
    WalletAppConfig(configs: _*)

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
