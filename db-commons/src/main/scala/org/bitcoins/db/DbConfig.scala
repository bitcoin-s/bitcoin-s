package org.bitcoins.db

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.util.BitcoinSLogger
import slick.basic.DatabaseConfig
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

/**
  * This is meant to encapsulate all of our database configuration.
  *
  * For each sub-project that needs a DB (currently wallet, node
  * and chain), there are 4 "network" databases. The following
  * directories are created under `$HOME/.bitcoin-s`
  *
  * <ol>
  *   <li>
  *     `mainnet` - this stores things related to the
  *     [[org.bitcoins.core.protocol.blockchain.MainNetChainParams MainNet]] network
  *   </li>
  *   <li>
  *     `testnet3` - this stores things related to the
  *     [[org.bitcoins.core.config.TestNet3 testnet3]] network
  *   </li>
  *   <li>
  *     `regtest` - this stores things related your local
  *     [[org.bitcoins.core.config.RegTest regtest]] network
  *   </li>
  *   <li>
  *     `unittest` - this stores things related to unit tests. Unit tests are free to
  *     create and destroy databases at will in this directory, so you should not
  *     store anything there.
  *   </li>
  * </ol>
  *
  * In order to create a database configuraion for a new project,
  * you must create a `.conf` file in the resource directory
  * of your project. This file must define the following configuration
  * settings:
  *
  * {{{
  *   specificDbSettings.dbName
  *   specificDbSettings.user
  * }}}
  *
  * You should include the following at the top of the file:
  *
  * {{{
  *   include "db.conf"
  * }}}
  *
  * You must then create objects for your config, similar to
  * what's been done in
  * [[org.bitcoins.chain.db.ChainDbConfig ChainDbConfig]].
  * The objects need to define the value `configPath`, this
  * is where we look for the configuration file.
  */
trait DbConfig extends BitcoinSLogger { this: NetworkDb =>

  /** The path we look for our configuration file in */
  def configPath: String

  /** The configuration details for connecting/using the database for our projects
    * that require datbase connections
    * */
  lazy val dbConfig: DatabaseConfig[SQLiteProfile] = {
    //if we don't pass specific class, non-deterministic
    //errors around the loaded configuration depending
    //on the state of the default classLoader
    //https://github.com/lightbend/config#debugging-your-configuration
    val dbConfig: DatabaseConfig[SQLiteProfile] = {
      val conf = ConfigFactory.load(configPath)
      logger.trace(s"conf: $conf")
      DatabaseConfig.forConfig(path = configKey, config = conf)
      // classLoader = getClass.getClassLoader)
    }

    logger.trace(s"class: ${getClass.getSimpleName}")
    logger.trace(s"Resolved DB config: ${dbConfig.config}")

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


/**
  * The network that a database is affiliated with
  */
sealed trait NetworkDb  {
  /** This is the key we look for in the config file
    * to identify a database database. An example
    * of this for the [[MainNetDbConfig]] is ''mainnetDb''
    * @return
    */
  def configKey: String

}


trait MainNetDbConfig extends NetworkDb {
  override lazy val configKey: String = "mainnetDb"
}

trait TestNet3DbConfig extends NetworkDb {
  override lazy val configKey: String = "testnet3Db"
}

trait RegTestDbConfig extends NetworkDb {
  override lazy val configKey: String = "regtestDb"
}

trait UnitTestDbConfig extends NetworkDb {
  override lazy val configKey: String = "unittestDb"

}
