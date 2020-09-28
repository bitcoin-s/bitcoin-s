package org.bitcoins.dlc.oracle

import java.nio.file.{Files, Path}

import com.typesafe.config.Config
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.{AppConfig, DbManagement, JdbcProfileComponent}
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.keymanager.WalletStorage

import scala.concurrent.{ExecutionContext, Future}

case class DLCOracleAppConfig(
    private val directory: Path,
    private val conf: Config*)(implicit val ec: ExecutionContext)
    extends AppConfig
    with DbManagement
    with JdbcProfileComponent[DLCOracleAppConfig] {

  import profile.api._

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

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    val numMigrations = {
      migrate()
    }

    logger.info(s"Applied $numMigrations to the wallet project")

    FutureUtil.unit
  }

  /** Checks if our oracle as a mnemonic seed associated with it */
  def seedExists(): Boolean = {
    WalletStorage.seedExists(seedPath)
  }

  def exists(): Boolean = {
    seedExists() &&
    Files.exists(baseDatadir.resolve("oracle.sqlite"))
  }

  private val rValueTable: TableQuery[Table[_]] = {
    RValueDAO()(ec, appConfig).table
  }

  private val eventTable: TableQuery[Table[_]] = {
    EventDAO()(ec, appConfig).table
  }

  private val eventOutcomeTable: TableQuery[Table[_]] = {
    EventOutcomeDAO()(ec, appConfig).table
  }

  override def allTables: List[TableQuery[Table[_]]] =
    List(rValueTable, eventTable, eventOutcomeTable)
}
