package org.bitcoins.db

import java.io.File

import com.typesafe.config.Config
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  MainNetChainParams,
  RegTestNetChainParams,
  TestNetChainParams
}
import org.bitcoins.core.util.BitcoinSLogger
import slick.basic.DatabaseConfig
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

/**
  * Created by chris on 9/11/16.
  *
  * This is meant to encapsulate all of our database configuration related things
  * There are currently 4 "network" databases that we have setup
  * 1. mainnet - this stores things related to the [[org.bitcoins.core.protocol.blockchain.MainNetChainParams MainNet]] network
  * 2. testnet3 - this stores things related to the [[org.bitcoins.core.config.TestNet3 testnet3]] network
  * 3. regtest - this stores things related your local [[org.bitcoins.core.config.RegTest regtest]] network
  * 4. unittest - this stores things related to unit tests. This is the database unit tests have
  *
  * An example of a project creating their own database configuration can be seen
  * in the node project. This project defines a database name, a database username,
  * and various other slick database configurations [[https://github.com/bitcoin-s/bitcoin-s-core/blob/016d45c672ec7ef15142516332d92dffd633960f/node/src/main/resources/application.conf#L33 here]]
  *
  * Each instance of [[DbConfig DbConfig]] has a [[configKey]] that indicates
  * how you read the database configurations from on the classpath. For instance,
  * the [[MainNetDbConfig]] has a [[configKey]] of ''mainnetDb''.
  *
  */
sealed abstract class DbConfig extends BitcoinSLogger {

  /** This is the key we look for in the config file
    * to identify a database database. An example
    * of this for the [[MainNetDbConfig]] is ''mainnetDb''
    * @return
    */
  def configKey: String

  /** The configuration details for connecting/using the database for our projects
    * that require datbase connections
    * */
  lazy val dbConfig: DatabaseConfig[SQLiteProfile] = {
    //if we don't pass specific class, non-deterministic
    //errors around the loaded configuration depending
    //on the state of the default classLoader
    //https://github.com/lightbend/config#debugging-your-configuration
    val dbConfig: DatabaseConfig[SQLiteProfile] = {
      DatabaseConfig.forConfig(path = configKey,
                               classLoader = getClass().getClassLoader())
    }

    createDbFileIfDNE(config = dbConfig.config)

    dbConfig
  }

  /** The database we are connecting to for our spv node */
  def database: Database = {
    dbConfig.db
  }

  private def createDbFileIfDNE(config: Config): Boolean = {
    val resolvedConfig = config.resolve()
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

object DbConfig {

  /**
    * Gets the correct DB config from the given chain params
    */
  def fromChainParams(chainParams: ChainParams): DbConfig = chainParams match {
    case MainNetChainParams    => MainNetDbConfig
    case TestNetChainParams    => TestNet3DbConfig
    case RegTestNetChainParams => RegTestDbConfig
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
