package org.bitcoins.db

import org.bitcoins.core.util.BitcoinSLogger
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

trait JdbcProfileComponent[+ConfigType <: DbAppConfig] extends BitcoinSLogger {

  def appConfig: ConfigType

  /**
    * The configuration details for connecting/using the database for our projects
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
}
