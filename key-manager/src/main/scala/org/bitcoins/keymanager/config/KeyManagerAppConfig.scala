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

  override def moduleName: String = KeyManagerAppConfig.moduleName

  override def baseDatadir: Path = directory

  lazy val networkParameters: NetworkParameters = chain.network

  lazy val walletNameOpt: Option[String] = {
    config.getStringOrNone(s"bitcoin-s.wallet.walletName")
  }

  lazy val seedFolder: Path = baseDatadir
    .resolve(WalletStorage.SEED_FOLDER_NAME)

  /** The path to our encrypted mnemonic seed */
  lazy val seedPath: Path = {
    val prefix = walletNameOpt match {
      case Some(walletName) =>
        s"$walletName-"
      case None => ""
    }

    seedFolder.resolve(s"$prefix${WalletStorage.ENCRYPTED_SEED_FILE_NAME}")
  }

  override def start(): Future[Unit] = {
    val oldDefaultFile =
      baseDatadir.resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)

    val newDefaultFile = seedFolder
      .resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)

    if (!Files.exists(newDefaultFile) && Files.exists(oldDefaultFile)) {
      // Copy key manager file to new location
      logger.info(s"Copying seed file to seeds folder $newDefaultFile")
      // Create directory
      Files.createDirectories(newDefaultFile.getParent)
      Files.copy(oldDefaultFile, newDefaultFile)
    }
    FutureUtil.unit
  }

  override def stop(): Future[Unit] = FutureUtil.unit

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
  override val moduleName: String = "keymanager"

  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): KeyManagerAppConfig =
    KeyManagerAppConfig(datadir, confs: _*)
}
