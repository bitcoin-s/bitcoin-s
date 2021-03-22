package org.bitcoins.db

import grizzled.slf4j.Logging
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.duration.Duration

trait JdbcProfileComponent[+ConfigType <: DbAppConfig] extends Logging {

  def appConfig: ConfigType

  /** The configuration details for connecting/using the database for our projects
    * that require database connections
    */
  lazy val dbConfig: DatabaseConfig[JdbcProfile] = {
    appConfig.slickDbConfig
  }

  lazy val profile: JdbcProfile = dbConfig.profile
  import profile.api._

  lazy val username: String = dbConfig.config.getString("db.user")

  lazy val password: String = dbConfig.config.getString("db.password")

  lazy val numThreads: Int = dbConfig.config.getInt("db.numThreads")

  /** The database we are connecting to */
  lazy val database: Database = {
    dbConfig.db
  }

  private[this] var hikariLoggerOpt: Option[HikariLogging] = None

  /** Starts the background logger for hikari
    * @param interval - how often hikari logs database connection pool information
    */
  protected def startHikariLogger(interval: Duration): HikariLogging = {
    hikariLoggerOpt match {
      case Some(hikarkiLogger) => hikarkiLogger
      case None                =>
        //this is needed to get the 'AsyncExecutor' bean below to register properly
        //dbConfig.database.ioExecutionContext
        val _ = database.ioExecutionContext
        //start a new one
        HikariLogging.fromJdbcProfileComponent(this, interval) match {
          case Some(hikariLogger) =>
            hikariLoggerOpt = Some(hikariLogger)
            hikariLogger
          case None =>
            sys.error(s"Could not start hikari logging")
        }
    }

  }

  protected def stopHikariLogger(): Unit = {
    hikariLoggerOpt.foreach(_.stop())
    ()
  }
}
