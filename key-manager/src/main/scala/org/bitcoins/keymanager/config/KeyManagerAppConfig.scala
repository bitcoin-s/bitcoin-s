package org.bitcoins.keymanager.config

import com.typesafe.config.Config
import org.bitcoins.commons.config.{AppConfig, AppConfigFactory, ConfigOps}
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.hd.{HDPurpose, HDPurposes}
import org.bitcoins.core.wallet.keymanagement.KeyManagerParams
import org.bitcoins.crypto.{AesPassword, CryptoUtil}
import org.bitcoins.keymanager.{ReadMnemonicError, WalletStorage}
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import scodec.bits.BitVector

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

  private lazy val defaultAccountKind: HDPurpose =
    config.getString("bitcoin-s.wallet.defaultAccountType") match {
      case "legacy"        => HDPurposes.Legacy
      case "segwit"        => HDPurposes.SegWit
      case "nested-segwit" => HDPurposes.NestedSegWit
      // todo: validate this pre-app startup
      case other: String =>
        throw new RuntimeException(s"$other is not a valid account type!")
    }

  /** Entropy provided by the a user in their bitcoin-s.conf
    * configuration file. This should be used to seed the keymanager
    * rather than randomly generating entropy.
    */
  private lazy val externalEntropy: Option[String] = {
    val opt = config.getStringOrNone("bitcoin-s.keymanager.entropy")
    opt
  }

  private val kmParams: KeyManagerParams = {
    KeyManagerParams(seedPath, defaultAccountKind, network)
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
      logger.info(
        s"Migrated keymanager seed from=${oldDefaultFile.toAbsolutePath} to=${newDefaultFile.toAbsolutePath}")
      Future.unit
    } else if (!Files.exists(newDefaultFile)) {
      initializeKeyManager()
    } else {
      logger.info(
        s"Starting keymanager with seedPath=${seedPath.toAbsolutePath}")
      Future.unit
    }
  }

  override def stop(): Future[Unit] = Future.unit

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

  /** Creates a [[BIP39KeyManager]] from the seed referenced by this [[KeyManagerAppConfig]]
    * with the given wallet purpose
    */
  def toBip39KeyManager: BIP39KeyManager = {
    val kmE: Either[ReadMnemonicError, BIP39KeyManager] =
      BIP39KeyManager.fromParams(kmParams = kmParams,
                                 passwordOpt = aesPasswordOpt,
                                 bip39PasswordOpt = bip39PasswordOpt)
    kmE match {
      case Left(err) =>
        sys.error(
          s"Could not create a BIP39KeyManager from the KeyManagerAppConfig, err=$err")
      case Right(km) =>
        km
    }
  }

  /** Initializes the key manager. Takes into consideration if external entropy
    * has been provided to bitcoin-s via the bitcoin-s.conf file
    */
  private def initializeKeyManager(): Future[Unit] = {
    val entropy: BitVector = externalEntropy match {
      case Some(entropy) =>
        logger.info(
          s"Initializing new mnemonic seed at path=${seedPath.toAbsolutePath} with external entropy")
        val hexOpt = BitVector.fromHex(entropy)
        hexOpt match {
          case Some(hex) => hex
          case None =>
            sys.error(
              s"Entropy provided by bitcoin-s.keymanager.entropy was not valid hex, got=${entropy}")
        }
      case None =>
        logger.info(
          s"Initializing new mnemonic seed at path=${seedPath.toAbsolutePath}")
        MnemonicCode.getEntropy256Bits
    }

    if (!CryptoUtil.checkEntropy(entropy)) {
      sys.error(
        s"The entropy used by bitcoin-s does not pass basic entropy sanity checks, got=$entropy")
    }

    val initE = BIP39KeyManager.initializeWithEntropy(
      aesPasswordOpt = aesPasswordOpt,
      entropy = entropy,
      bip39PasswordOpt = bip39PasswordOpt,
      kmParams = kmParams)
    initE match {
      case Right(km) =>
        logger.info(
          s"Successfully initialize seed at path with root xpub=${km.getRootXPub}")
        Future.unit
      case Left(err) =>
        Future.failed(
          new RuntimeException(
            s"Failed to initialize mnemonic seed in keymanager with err=$err"))
    }
  }
}

object KeyManagerAppConfig extends AppConfigFactory[KeyManagerAppConfig] {
  override val moduleName: String = "keymanager"

  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): KeyManagerAppConfig =
    KeyManagerAppConfig(datadir, confs: _*)
}
