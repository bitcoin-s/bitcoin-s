package org.bitcoins.dlc.oracle.config

import com.typesafe.config.Config
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.ExtKeyVersion.SegWitMainNetPriv
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.protocol.tlv.EnumEventDescriptorV0TLV
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.keymanagement.KeyManagerParams
import org.bitcoins.crypto.AesPassword
import org.bitcoins.db.DatabaseDriver._
import org.bitcoins.db._
import org.bitcoins.dlc.oracle.DLCOracle
import org.bitcoins.dlc.oracle.storage._
import org.bitcoins.keymanager.WalletStorage
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.keymanager.config.KeyManagerAppConfig

import java.nio.file.{Files, Path}
import scala.concurrent.{ExecutionContext, Future}

case class DLCOracleAppConfig(
    private val directory: Path,
    private val confs: Config*)(implicit val ec: ExecutionContext)
    extends DbAppConfig
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

  lazy val kmConf: KeyManagerAppConfig =
    KeyManagerAppConfig(directory, confs: _*)

  lazy val networkParameters: NetworkParameters = chain.network

  /** The path to our encrypted mnemonic seed */
  lazy val seedPath: Path = kmConf.seedPath

  override def start(): Future[Unit] = {
    logger.debug(s"Initializing dlc oracle setup")
    super.start().flatMap { _ =>
      if (Files.notExists(datadir)) {
        Files.createDirectories(datadir)
      }
      val numMigrations = {
        migrate()
      }
      logger.info(s"Applied $numMigrations to the dlc oracle project")

      val migrations = migrationsApplied()
      if (migrations == 2 || migrations == 3) { // For V2/V3 migrations
        logger.debug(s"Doing V2/V3 Migration")

        val dummyMigrationTLV = EnumEventDescriptorV0TLV.dummy

        val eventDAO = EventDAO()(ec, appConfig)
        for {
          // get all old events
          allEvents <- eventDAO.findByEventDescriptor(dummyMigrationTLV)
          allOutcomes <- EventOutcomeDAO()(ec, appConfig).findAll()

          outcomesByNonce = allOutcomes.groupBy(_.nonce)
          // Update them to have the correct event descriptor
          updated = allEvents.map { eventDb =>
            val outcomeDbs = outcomesByNonce(eventDb.nonce)
            val descriptor =
              EventOutcomeDbHelper.createEnumEventDescriptor(outcomeDbs)
            eventDb.copy(eventDescriptorTLV = descriptor)
          }

          _ <- eventDAO.upsertAll(updated)
        } yield ()
      } else FutureUtil.unit
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

  lazy val kmParams: KeyManagerParams =
    KeyManagerParams(kmConf.seedPath,
                     HDPurpose(DLCOracle.R_VALUE_PURPOSE),
                     network)

  lazy val aesPasswordOpt: Option[AesPassword] = kmConf.aesPasswordOpt
  lazy val bip39PasswordOpt: Option[String] = kmConf.bip39PasswordOpt

  /** Checks if our oracle as a mnemonic seed associated with it */
  def seedExists(): Boolean = kmConf.seedExists()

  def exists(): Boolean = {
    lazy val hasDb = this.driver match {
      case PostgreSQL => true
      case SQLite =>
        Files.exists(datadir.resolve("oracle.sqlite"))
    }
    seedExists() && hasDb
  }

  def initialize(): Future[DLCOracle] = {
    if (!seedExists()) {
      BIP39KeyManager.initialize(aesPasswordOpt = aesPasswordOpt,
                                 kmParams = kmParams,
                                 bip39PasswordOpt = bip39PasswordOpt) match {
        case Left(err) => sys.error(err.toString)
        case Right(_) =>
          logger.info("Successfully generated a seed and key manager")
      }
    }

    val key =
      WalletStorage.getPrivateKeyFromDisk(kmConf.seedPath,
                                          SegWitMainNetPriv,
                                          aesPasswordOpt,
                                          bip39PasswordOpt)
    val oracle = DLCOracle(key)(this)

    start().map(_ => oracle)
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
