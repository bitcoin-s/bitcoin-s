package org.bitcoins.db

import com.typesafe.config.*
import org.bitcoins.commons.config.*
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import java.nio.file.{Path, Paths}
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.{Failure, Success, Try}

abstract class DbAppConfig extends AppConfig {
  implicit def ec: ExecutionContext
  private val isStarted: AtomicBoolean = new AtomicBoolean(false)

  override def start(): Future[Unit] = {
    super.start().map { _ =>
      logger.info(s"Done super.start() DbAppConfig")
      if (isStarted.compareAndSet(false, true)) {
        logger.info(s"Starting DbAppConfig for module=${moduleName}")
        // initialize the database connection pool
        slickDbConfig
      } else {
        logger.info(s"${getClass.getSimpleName} was already started")
      }
    }
  }

  /** Releases the thread pool associated with this AppConfig's DB */
  override def stop(): Future[Unit] = {
    if (isStarted.compareAndSet(true, false)) {
      logger.info(s"Stopping DbAppConfig for module=${moduleName}")
      slickDbConfigOpt match {
        case None => Future.unit
        case Some(c) =>
          slickDbConfigOpt = None
          Future {
            blocking { c.db.close() }
          }
      }
    } else {
      logger.info(s"${getClass.getSimpleName} was already stopped")
      Future.unit
    }
  }

  lazy val dbUsername: String =
    config.getString(s"bitcoin-s.$moduleName.db.user")

  lazy val dbPassword: String =
    config.getString(s"bitcoin-s.$moduleName.db.password")

  private lazy val busyTimeout: FiniteDuration = {
    val durationOpt =
      config.getDurationOpt(s"bitcoin-s.$moduleName.db.busy-timeout")
    durationOpt match {
      case Some(d) => FiniteDuration(d.toMillis, TimeUnit.MILLISECONDS)
      case None    => FiniteDuration(30, TimeUnit.SECONDS)
    }
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
        // journal_mode=WAL is set in the URL so it applies at the JDBC driver
        // level on every connection, regardless of whether HikariCP or Slick's
        // disabled connection pool (plain JDBC) is in use.
        // connectionInitSql is HikariCP-specific and has no effect when
        // connectionPool = disabled.

        s""""jdbc:sqlite:${AppConfig
            .safePathToString(dbPath)
            .replace(
              "\"",
              "")}/$dbName?journal_mode=WAL&transaction_mode=IMMEDIATE&busy_timeout=${busyTimeout.toMillis}""""
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

  private var slickDbConfigOpt: Option[DatabaseConfig[JdbcProfile]] = None
  def slickDbConfig: DatabaseConfig[JdbcProfile] = {
    if (slickDbConfigOpt.isEmpty && isStarted.get()) {
      logger.info(s"Starting database connection pool for module=$moduleName")

      val overrideConf = ConfigFactory.parseString {
        s"""
           |bitcoin-s {
           |  $moduleName {
           |     db {
           |        path = ${AppConfig.safePathToString(dbPath)}
           |        name = $dbName
           |        user = "$dbUsername"
           |        password = "$dbPassword"
           |        url = $jdbcUrl
           |        registerMbeans = false
           |     }
           |  }
           |}
      """.stripMargin
      }

      val usedConf = overrideConf.withFallback(config)
      Try {
        val c =
          DatabaseConfig.forConfig[JdbcProfile](path = s"bitcoin-s.$moduleName",
                                                config = usedConf)
        // don't log entire config in prod due to secrets such as password
        // may be useful for debugging in dev, but be careful in prod
//        logger.trace(
//          s"Resolved DB config: ${ConfigOps(c.config).asReadableJson}")
        slickDbConfigOpt = Some(c)
        slickDbConfigOpt.get
      } match {
        case Success(value) =>
          value
        case Failure(exception) =>
          logger.error(s"Error when loading database from config: $exception")
          // don't log entire config in prod due to secrets such as password
          // may be useful for debugging in dev, but be careful in prod
//          logger.error(s"Configuration: ${usedConf.asReadableJson}")
          throw exception
      }
    } else if (slickDbConfigOpt.isDefined && isStarted.get()) {
      slickDbConfigOpt.get
    } else {
      sys.error(
        s"Cannot get slickDbConfig for module=$moduleName because the config is not started yet")
    }
  }

  lazy val isHikariLoggingEnabled: Boolean = {
    val hikariLoggingOpt =
      config.getBooleanOpt(s"bitcoin-s.$moduleName.hikari-logging")
    hikariLoggingOpt match {
      case Some(bool) => bool
      case None       =>
        // default hikari logging off
        false
    }
  }

  /** Gets how often we should log hikari connection pool stats if None, this
    * means [[isHikariLoggingEnabled]] is not enabled
    */
  lazy val hikariLoggingInterval: Option[Duration] = {
    if (isHikariLoggingEnabled) {
      val intervalOpt =
        config.getDurationOpt(s"bitcoin-s.$moduleName.hikari-logging-interval")
      val interval = intervalOpt match {
        case Some(interval) => interval
        case None           =>
          // default to 1 minute if nothing is set
          new FiniteDuration(1, TimeUnit.MINUTES)
      }
      Some(interval)
    } else {
      None
    }

  }
}
