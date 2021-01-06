package org.bitcoins.db

import com.typesafe.config._
import org.bitcoins.db.AppConfig.safePathToString
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import java.nio.file.{Path, Paths}
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

abstract class DbAppConfig extends AppConfig {

  /** Releases the thread pool associated with this AppConfig's DB */
  override def stop(): Future[Unit] = {
    Future.successful(slickDbConfig.db.close())
  }

  lazy val driver: DatabaseDriver = {
    val driverStr =
      getConfigString(s"bitcoin-s.$moduleName.db.driver").toLowerCase
    if (driverStr.contains("sqlite")) {
      SQLite
    } else if (driverStr.contains("postgres")) {
      PostgreSQL
    } else {
      sys.error(s"Could not find a DatabaseDriver for string=$driverStr")
    }
  }

  lazy val jdbcUrl: String = {
    driver match {
      case SQLite =>
        s""""jdbc:sqlite:"${safePathToString(dbPath)}/$dbName"""
      case PostgreSQL =>
        s""""jdbc:postgresql://$dbHost:$dbPort/$dbName""""
    }
  }

  /** The path where our DB is located */
  lazy val dbPath: Path = {
    val pathStrOpt =
      getConfigStringOpt(s"bitcoin-s.$moduleName.db.path")
    pathStrOpt match {
      case Some(pathStr) =>
        Paths.get(pathStr)
      case None =>
        sys.error(s"Could not find dbPath for $moduleName.db.path")
    }
  }

  /** The name of our database */
  lazy val dbName: String = {
    val name = getConfigString(s"bitcoin-s.$moduleName.db.name")

    driver match {
      case SQLite     => s"$name.sqlite"
      case PostgreSQL => name
    }
  }

  /** The host of our postgresql database */
  lazy val dbHost: String = {
    val hostOpt = getConfigStringOpt(s"bitcoin-s.$moduleName.db.host")

    hostOpt match {
      case Some(host) => host
      case None       => "localhost"
    }
  }

  /** The port number of our postgresql database */
  lazy val dbPort: Int = {
    val portOpt = config.getIntOpt(s"bitcoin-s.$moduleName.db.port")

    portOpt match {
      case Some(port) => port
      case None       => 5432 // default postgres port
    }
  }

  lazy val schemaName: Option[String] = driver match {
    case PostgreSQL =>
      Some(moduleName)
    case SQLite =>
      None
  }

  lazy val slickDbConfig: DatabaseConfig[JdbcProfile] = {
    // Create overrides if modules want to change their path or db name
    val overrideConf = ConfigFactory.parseString {
      s"""
         |bitcoin-s {
         |  $moduleName {
         |     db {
         |        path = ${safePathToString(dbPath)}
         |        name = $dbName
         |        url = $jdbcUrl
         |     }
         |  }
         |}
      """.stripMargin
    }

    val usedConf = overrideConf.withFallback(config)
    Try {
      val c = DatabaseConfig.forConfig[JdbcProfile](path =
                                                      s"bitcoin-s.$moduleName",
                                                    config = usedConf)

      logger.trace(s"Resolved DB config: ${ConfigOps(c.config).asReadableJson}")
      c
    } match {
      case Success(value) =>
        value
      case Failure(exception) =>
        logger.error(s"Error when loading database from config: $exception")
        logger.error(s"Configuration: ${usedConf.asReadableJson}")
        throw exception
    }
  }
}
