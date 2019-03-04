package org.bitcoins.db

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

  /** The configuration details for connecting/using the database for our projects
    * that require datbase connections
    * */
  def dbConfig: DatabaseConfig[SQLiteProfile] = {
    //if we don't pass specific class, non-deterministic
    //errors around the loaded configuration depending
    //on the state of the default classLoader
    //https://github.com/lightbend/config#debugging-your-configuration
    DatabaseConfig.forConfig(path = configKey,
                             classLoader = getClass().getClassLoader())
  }

  /** The database we are connecting to for our spv node */
  def database: Database = {
    createDbFileIfDNE()
    dbConfig.db
  }

  private def createDbFileIfDNE(): Boolean = {
    val resolvedConfig = dbConfig.config.resolve()
    logger.info(s"DbConfig=${dbConfig.config.root().render()}")
    //should add a check in here that we are using sqlite
    val dbPath = new File(resolvedConfig.getString("dbPath"))
    if (!dbPath.exists()) {
      logger.info(s"Creating database directory=${dbPath.getAbsolutePath}")
      dbPath.mkdirs()
    } else {
      true
    }
  }
}

sealed abstract class MainNetDbConfig extends DbConfig {
  override lazy val configKey: String = "mainnetDb"
}

object MainNetDbConfig extends MainNetDbConfig

sealed abstract class TestNet3DbConfig extends DbConfig {
  override lazy val configKey: String = "testnet3Db"
}

object TestNet3DbConfig extends TestNet3DbConfig

sealed abstract class RegTestDbConfig extends DbConfig {
  override lazy val configKey: String = "regtestDb"
}

object RegTestDbConfig extends RegTestDbConfig

sealed abstract class UnitTestDbConfig extends DbConfig {
  override lazy val configKey: String = "unittestDb"

}

object UnitTestDbConfig extends UnitTestDbConfig
