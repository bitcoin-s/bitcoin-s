package org.bitcoins.node.db

import slick.basic.DatabaseConfig
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

/**
  * Created by chris on 9/11/16.
  */
trait DbConfig {

  /** The configuration details for connecting/using the database for our spv node */
  def dbConfig: DatabaseConfig[PostgresProfile]

  /** The database we are connecting to for our spv node */
  def database: Database = dbConfig.db
}

trait MainNetDbConfig extends DbConfig {
  override def dbConfig: DatabaseConfig[PostgresProfile] =
    DatabaseConfig.forConfig("databaseUrl")
}
object MainNetDbConfig extends MainNetDbConfig

trait TestNet3DbConfig extends DbConfig {
  override def dbConfig: DatabaseConfig[PostgresProfile] =
    DatabaseConfig.forConfig("testNet3DatabaseUrl")
}
object TestNet3DbConfig extends TestNet3DbConfig

trait RegTestDbConfig extends DbConfig {
  override def dbConfig: DatabaseConfig[PostgresProfile] =
    DatabaseConfig.forConfig("regTestDatabaseUrl")
}
object RegTestDbConfig extends RegTestDbConfig


trait UnitTestDbConfig extends DbConfig {
  /** Reads the configuration for the database specified inside of application.conf */
  lazy val dbConfig: DatabaseConfig[PostgresProfile] =
    DatabaseConfig.forConfig("unitTestDatabaseUrl")
}

object UnitTestDbConfig extends UnitTestDbConfig