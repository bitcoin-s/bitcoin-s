package org.bitcoins.db

import java.nio.file.{Files, Path, Paths}

import org.bitcoins.core.util.BitcoinSLogger
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.util.{Failure, Success, Try}

trait JdbcProfileComponent[+ConfigType <: AppConfig] extends BitcoinSLogger {

  def appConfig: ConfigType

  /**
    * The configuration details for connecting/using the database for our projects
    * that require datbase connections
    */
  val dbConfig: DatabaseConfig[JdbcProfile] = {
    val slickDbConfig = {
      Try {
        DatabaseConfig.forConfig[JdbcProfile](path = appConfig.moduleName,
                                              config = appConfig.config)
      } match {
        case Success(value) =>
          value
        case Failure(exception) =>
          logger.error(s"Error when loading database from config: $exception")
          logger.error(s"Configuration: ${appConfig.config.asReadableJson}")
          throw exception
      }
    }

    logger.debug(s"Resolved DB config: ${slickDbConfig.config.asReadableJson}")

    val _ = createDbFileIfDNE()

    slickDbConfig
  }

  val profile: JdbcProfile = dbConfig.profile
  import profile.api._

  lazy val jdbcUrl: String = {
    dbConfig.config.getString("db.url")
  }

  /** The database we are connecting to */
  lazy val database: Database = {
    dbConfig.db
  }

  /** The path where our DB is located */
  lazy val dbPath: Path = {
    val pathStr = appConfig.config.getString(s"${appConfig.moduleName}.db.path")
    val path = Paths.get(pathStr)
    logger.debug(s"DB path: $path")
    path
  }

  /** The name of our database */
  // todo: what happens to this if we
  // dont use SQLite?
  lazy val dbName: String = {
    appConfig.config.getString(s"${appConfig.moduleName}.db.name")
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
