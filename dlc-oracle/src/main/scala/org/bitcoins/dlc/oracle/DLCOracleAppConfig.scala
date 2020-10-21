package org.bitcoins.dlc.oracle

import java.nio.file.{Files, Path}

import com.typesafe.config.Config
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ExtKeyVersion.SegWitMainNetPriv
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto.AesPassword
import org.bitcoins.db.DatabaseDriver._
import org.bitcoins.db._
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.keymanager.{DecryptedMnemonic, WalletStorage}

import scala.concurrent.{ExecutionContext, Future}

case class DLCOracleAppConfig(
    private val directory: Path,
    private val confs: Config*)(implicit val ec: ExecutionContext)
    extends AppConfig
    with DbManagement
    with JdbcProfileComponent[DLCOracleAppConfig] {

  import profile.api._

  override def configOverrides: List[Config] = confs.toList

  override def appConfig: DLCOracleAppConfig = this

  override type ConfigType = DLCOracleAppConfig

  override def newConfigOfType(
      configOverrides: Seq[Config]): DLCOracleAppConfig =
    DLCOracleAppConfig(directory, configOverrides: _*)

  override def moduleName: String = "oracle"

  override def baseDatadir: Path = directory

  lazy val networkParameters: NetworkParameters = chain.network

  /** The path to our encrypted mnemonic seed */
  lazy val seedPath: Path = {
    baseDatadir.resolve(WalletStorage.ENCRYPTED_SEED_FILE_NAME)
  }

  override def start(): Future[Unit] = {
    logger.debug(s"Initializing wallet setup")
    for {
      _ <- super.start()
    } yield {
      if (Files.notExists(datadir)) {
        Files.createDirectories(datadir)
      }
      val numMigrations = {
        migrate()
      }
      logger.info(s"Applied $numMigrations to the dlc oracle project")
    }
  }

  def serverConf: Config = {
    config.getConfig("bitcoin-s.server")
  }

  def rpcPortOpt: Option[Int] = {
    if (serverConf.hasPath("bitcoin-s.server.rpcport")) {
      Some(serverConf.getInt("bitcoin-s.server.rpcport"))
    } else {
      None
    }
  }

  /** Checks if our oracle as a mnemonic seed associated with it */
  def seedExists(): Boolean = {
    WalletStorage.seedExists(seedPath)
  }

  def exists(): Boolean = {
    lazy val hasDb = this.driver match {
      case PostgreSQL => true
      case SQLite =>
        Files.exists(datadir.resolve("oracle.sqlite"))
    }
    seedExists() && hasDb
  }

  def initialize(oracle: DLCOracle): Future[DLCOracle] = {
    start().map(_ => oracle)
  }

  def initialize(
      password: AesPassword,
      bip39PasswordOpt: Option[String] = None): Future[DLCOracle] = {
    if (!seedExists()) {
      val entropy = MnemonicCode.getEntropy256Bits
      val mnemonicCode = MnemonicCode.fromEntropy(entropy)
      val decryptedMnemonic = DecryptedMnemonic(mnemonicCode, TimeUtil.now)
      val encrypted = decryptedMnemonic.encrypt(password)
      WalletStorage.writeMnemonicToDisk(seedPath, encrypted)
    }

    val key =
      WalletStorage.getPrivateKeyFromDisk(seedPath,
                                          SegWitMainNetPriv,
                                          password,
                                          bip39PasswordOpt)
    val oracle = DLCOracle(key)(this)
    initialize(oracle)
  }

  private lazy val rValueTable: TableQuery[Table[_]] = {
    RValueDAO()(ec, appConfig).table
  }

  private lazy val eventTable: TableQuery[Table[_]] = {
    EventDAO()(ec, appConfig).table
  }

  private lazy val eventOutcomeTable: TableQuery[Table[_]] = {
    EventOutcomeDAO()(ec, appConfig).table
  }

  override def allTables: List[TableQuery[Table[_]]] =
    List(rValueTable, eventTable, eventOutcomeTable)
}

object DLCOracleAppConfig extends AppConfigFactory[DLCOracleAppConfig] {

  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): DLCOracleAppConfig =
    DLCOracleAppConfig(datadir, confs: _*)
}
