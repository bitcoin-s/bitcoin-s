package org.bitcoins.db

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config.NetworkParameters
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
import scala.util.Success
import scala.util.Try
import scala.util.Failure
import com.typesafe.config.ConfigRenderOptions

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
trait DbConfig extends BitcoinSLogger {

  /** The path we look for our configuration file in */
  def configPath: String

  /**
    * Removes some uninteresting parts of the config.
    * The `defaults()` at the end pretty-prints the
    * config, with comments explaining where different
    * parts come from. If replacing with `.concise`
    * the comments are removed, and a non-pretty-printed
    * JSON representation is outputted.
    */
  private def renderConfigWithoutFluff(config: Config): String =
    config
      .withoutPath("akka")
      .withoutPath("java")
      .withoutPath("sun")
      .withoutPath("awt")
      .withoutPath("file")
      .withoutPath("jline")
      .withoutPath("jna")
      .withoutPath("jnidispatch")
      .withoutPath("line")
      .withoutPath("user")
      .withoutPath("path")
      .withoutPath("os")
      .withoutPath("ssl-config")
      .root()
      .render(ConfigRenderOptions.defaults())

  /** The configuration details for connecting/using the database for our projects
    * that require datbase connections
    * */
  lazy val dbConfig: DatabaseConfig[SQLiteProfile] = {
    //if we don't pass specific class, non-deterministic
    //errors around the loaded configuration depending
    //on the state of the default classLoader
    //https://github.com/lightbend/config#debugging-your-configuration
    val dbConfig = {
      val conf = ConfigFactory.load(configPath)
      val configKey = networkDb.configKey
      Try {
        val confAtKey = conf.getConfig(configKey)
        logger.trace(
          s"conf at key $configKey: ${renderConfigWithoutFluff(confAtKey)}")
        DatabaseConfig.forConfig[SQLiteProfile](path = configKey, config = conf)
      } match {
        case Success(value) =>
          value
        case Failure(exception) =>
          logger.error(s"Error when loading database from config: $exception")
          logger.error(s"Configuration: ${renderConfigWithoutFluff(conf)}")
          throw exception
      }
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

  /** The network associated with this db config */
  def networkDb: NetworkDb
}

/**
  * The network that a database is affiliated with
  */
sealed trait NetworkDb {

  /** This is the key we look for in the config file
    * to identify a database database. An example
    * of this for the [[NetworkDb.MainNetDbConfig]] is ''mainnetDb''
    * @return
    */
  def configKey: String

  def chain: ChainParams

  lazy val network: NetworkParameters = chain.network

}

object NetworkDb {

  object MainNetDbConfig extends NetworkDb {
    override lazy val configKey: String = "mainnetDb"

    override lazy val chain: MainNetChainParams.type = MainNetChainParams
  }

  object TestNet3DbConfig extends NetworkDb {
    override lazy val configKey: String = "testnet3Db"

    override lazy val chain: TestNetChainParams.type = TestNetChainParams
  }

  object RegTestDbConfig extends NetworkDb {
    override lazy val configKey: String = "regtestDb"

    override lazy val chain: RegTestNetChainParams.type = RegTestNetChainParams
  }

  object UnitTestDbConfig extends NetworkDb {
    override lazy val configKey: String = "unittestDb"

    override lazy val chain: RegTestNetChainParams.type = RegTestNetChainParams

  }
}
