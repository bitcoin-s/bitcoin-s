package org.bitcoins.node.db

import slick.basic.DatabaseConfig
import slick.jdbc.{SQLiteProfile}
import slick.jdbc.SQLiteProfile.api._

/**
  * Created by chris on 9/11/16.
  */
sealed abstract class DbConfig {

  /** The configuration details for connecting/using the database for our spv node */
  def dbConfig: DatabaseConfig[SQLiteProfile]

  /** The database we are connecting to for our spv node */
  def database: Database = dbConfig.db
}

sealed abstract class MainNetDbConfig extends DbConfig {
  override def dbConfig: DatabaseConfig[SQLiteProfile] =
    DatabaseConfig.forConfig("databaseUrl")
}
object MainNetDbConfig extends MainNetDbConfig

sealed abstract class TestNet3DbConfig extends DbConfig {
  override def dbConfig: DatabaseConfig[SQLiteProfile] =
    DatabaseConfig.forConfig("testNet3DatabaseUrl")
}
object TestNet3DbConfig extends TestNet3DbConfig

sealed abstract class RegTestDbConfig extends DbConfig {
  override def dbConfig: DatabaseConfig[SQLiteProfile] =
    DatabaseConfig.forConfig("regTestDatabaseUrl")
}
object RegTestDbConfig extends RegTestDbConfig

sealed abstract class UnitTestDbConfig extends DbConfig {

  /** Reads the configuration for the database specified inside of application.conf */
  override def dbConfig: DatabaseConfig[SQLiteProfile] =
    DatabaseConfig.forConfig("unitTestDatabaseUrl")
}

object UnitTestDbConfig extends UnitTestDbConfig
