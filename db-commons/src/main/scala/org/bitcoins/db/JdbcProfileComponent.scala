package org.bitcoins.db

import java.nio.file.{Files, Path, Paths}

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.DatabaseDriver._
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

trait JdbcProfileComponent[+ConfigType <: AppConfig] extends BitcoinSLogger {

  def appConfig: ConfigType

  /**
    * The configuration details for connecting/using the database for our projects
    * that require datbase connections
    */
  lazy val dbConfig: DatabaseConfig[JdbcProfile] = {
    val slickDbConfig = appConfig.slickDbConfig
    val _ = createDbFileIfDNE()

    slickDbConfig
  }

  lazy val profile: JdbcProfile = dbConfig.profile
  import profile.api._

  lazy val jdbcUrl: String = {
    dbConfig.config.getString("db.url")
  }

  lazy val driverName: String = {
    val parts = jdbcUrl.split(":")
    require(parts.size >= 2 && parts(0) == "jdbc",
            s"`${jdbcUrl}` must be a valid JDBC URL")
    parts(1)
  }

  lazy val schemaName: Option[String] = driver match {
    case PostgreSQL =>
      Some(appConfig.moduleName)
    case SQLite =>
      None
  }

  lazy val driver: DatabaseDriver = DatabaseDriver.fromString(driverName)

  lazy val username: String = dbConfig.config.getString("db.user")

  lazy val password: String = dbConfig.config.getString("db.password")

  lazy val numThreads: Int = dbConfig.config.getInt("db.numThreads")

  /** The database we are connecting to */
  lazy val database: Database = {
    dbConfig.db
  }

  /** The path where our DB is located */
  lazy val dbPath: Path = {
    val pathStrOpt =
      appConfig.getConfigStringOpt(s"bitcoin-s.${appConfig.moduleName}.db.path")
    pathStrOpt match {
      case Some(pathStr) =>
        val path = Paths.get(pathStr)
        path
      case None =>
        sys.error(s"Could not find dbPath for ${appConfig.moduleName}.db.path")
    }

  }

  /** The name of our database */
  // todo: what happens to this if we
  // dont use SQLite?
  lazy val dbName: String = {
    val path = s"bitcoin-s.${appConfig.moduleName}.db.name"
    val dbNameOpt =
      appConfig.getConfigStringOpt(path)
    dbNameOpt match {
      case Some(dbName) => dbName
      case None =>
        sys.error(s"Could not find dbname with path=$path")
    }
  }

  private def createDbFileIfDNE(): Unit = {
    //should add a check in here that we are using sqlite
    if (!Files.exists(dbPath)) {
      val _ = {
        logger.debug(s"Creating database directory=$dbPath")
        Files.createDirectories(dbPath)
        val dbFilePath = dbPath.resolve(dbName)
        logger.debug(s"Creating database file=$dbFilePath")
        Files.createFile(dbFilePath)
      }

      ()
    }
  }
}
