package org.bitcoins.node.db

import java.io.File

import org.bitcoins.core.util.BitcoinSLogger
import slick.basic.DatabaseConfig
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

/**
  * Created by chris on 9/11/16.
  */
sealed abstract class DbConfig extends BitcoinSLogger {

  def configKey: String

  /** The configuration details for connecting/using the database for our spv node */
  def dbConfig: DatabaseConfig[SQLiteProfile] = {
    DatabaseConfig.forConfig(configKey)
  }

  /** The database we are connecting to for our spv node */
  def database: Database = dbConfig.db

  protected def createDbFileIfDNE(): Boolean = {
    val resolvedConfig = dbConfig.config.resolve()
    //should add a check in here that we are using sqlite
    val dbPath = new File(resolvedConfig.getString("dbPath"))
    val dbName = dbConfig.config.getString("dbName")

    dbPath.mkdirs()
  }
}

sealed abstract class MainNetDbConfig extends DbConfig {
  override lazy val configKey: String = "mainnetDb"
}

object MainNetDbConfig extends MainNetDbConfig {
  createDbFileIfDNE()
}

sealed abstract class TestNet3DbConfig extends DbConfig {
  override lazy val configKey: String = "testnet3Db"
}

object TestNet3DbConfig extends TestNet3DbConfig {
  createDbFileIfDNE()
}

sealed abstract class RegTestDbConfig extends DbConfig {
  override lazy val configKey: String = "regtestDb"
}

object RegTestDbConfig extends RegTestDbConfig {
  createDbFileIfDNE()
}

sealed abstract class UnitTestDbConfig extends DbConfig {
  override lazy val configKey: String = "unittestDb"

}

object UnitTestDbConfig extends UnitTestDbConfig {
  createDbFileIfDNE()
}
