package org.bitcoins.keymanager.config

import com.typesafe.config.Config
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.AesPassword
import org.bitcoins.db._
import org.bitcoins.keymanager.WalletStorage

import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}

case class KeyManagerAppConfig(
    private val directory: Path,
    private val confs: Config*)(implicit val ec: ExecutionContext)
    extends AppConfig {

  override def configOverrides: List[Config] = confs.toList

  override type ConfigType = KeyManagerAppConfig

  override def newConfigOfType(
      configOverrides: Seq[Config]): KeyManagerAppConfig =
    KeyManagerAppConfig(directory, configOverrides: _*)

  override def moduleName: String = "keymanager"

  override def baseDatadir: Path = directory

  lazy val networkParameters: NetworkParameters = chain.network

  lazy val walletNameOpt: Option[String] = {
    config.getStringOrNone(s"bitcoin-s.wallet.walletName")
  }

  /** The path to our encrypted mnemonic seed */
  lazy val seedPath: Path = {
    val prefix = walletNameOpt match {
      case Some(walletName) =>
        s"$walletName-"
      case None => ""
    }
    baseDatadir
      .resolve(WalletStorage.SEED_FOLDER_NAME)
      .resolve(s"$prefix${WalletStorage.ENCRYPTED_SEED_FILE_NAME}")
  }

  override def start(): Future[Unit] = {
    walletNameOpt match {
      case Some(_) => FutureUtil.unit
      case None =>
        if (!seedExists()) {
          val defaultFile =
            baseDatadir.resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
          // Copy key manager file to new location
          if (WalletStorage.seedExists(defaultFile)) {
            logger.info(s"Copying seed file to seeds folder $seedPath")
            // Create directory
            Files.createDirectories(seedPath.getParent)
            Files.copy(defaultFile, seedPath)
          }
        }
        FutureUtil.unit
    }
  }

  lazy val aesPasswordOpt: Option[AesPassword] = {
    val passOpt = config.getStringOrNone(s"bitcoin-s.$moduleName.aesPassword")
    passOpt.flatMap(AesPassword.fromStringOpt)
  }

  lazy val bip39PasswordOpt: Option[String] = {
    config.getStringOrNone(s"bitcoin-s.$moduleName.bip39password")
  }

  /** Checks if our key manager as a mnemonic seed associated with it */
  def seedExists(): Boolean = {
    WalletStorage.seedExists(seedPath)
  }
}

object KeyManagerAppConfig extends AppConfigFactory[KeyManagerAppConfig] {

  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): KeyManagerAppConfig =
    KeyManagerAppConfig(datadir, confs: _*)
}
