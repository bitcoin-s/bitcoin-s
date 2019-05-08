package org.bitcoins.db

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.protocol.blockchain.ChainParams
import java.nio.file.Path
import java.nio.file.Paths
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.config.RegTest
import com.typesafe.config._
import org.bitcoins.core.util.BitcoinSLogger
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import slick.basic.DatabaseConfig
import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.bitcoins.core.protocol.blockchain.TestNetChainParams
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import java.nio.file.Files

abstract class AppConfig extends BitcoinSLogger {

  protected val configOverrides: List[Config] = List.empty

  def withOverrides(config: Config, configs: Config*): AppConfig = {
    // the two val assignments below are workarounds
    // for awkward name resolution in the block below
    val firstOverride = config
    val moduleName = this.moduleConfigName

    val numOverrides = configs.length + 1

    if (logger.isDebugEnabled()) {
      // force lazy evaluation before we print
      // our lines
      val oldConfStr = this.config.asReadableJson

      logger.debug(s"Creating AppConfig with $numOverrides override(s) ")
      logger.debug(s"Old config:")
      logger.debug(oldConfStr)
    }

    val newConf = new AppConfig {
      override val configOverrides = List(firstOverride) ++ configs
      override val moduleConfigName: String = moduleName
    }

    // to avoid non-necessary lazy load
    if (logger.isDebugEnabled()) {
      // force lazy load before we print
      val newConfStr = newConf.config.asReadableJson

      logger.debug("New config:")
      logger.debug(newConfStr)
    }

    newConf
  }

  /**
    * Name of module specific
    * config file. `wallet.conf`, `node.conf`,
    * etc.
    */
  protected def moduleConfigName: String

  /** The configuration details for connecting/using the database for our projects
    * that require datbase connections
    *
    */
  lazy val dbConfig: DatabaseConfig[SQLiteProfile] = {
    //if we don't pass specific class, non-deterministic
    //errors around the loaded configuration depending
    //on the state of the default classLoader
    //https://github.com/lightbend/config#debugging-your-configuration
    val dbConfig = {
      Try {
        DatabaseConfig.forConfig[SQLiteProfile](path = "database", config)
      } match {
        case Success(value) =>
          value
        case Failure(exception) =>
          logger.error(s"Error when loading database from config: $exception")
          logger.error(s"Configuration: ${config.asReadableJson}")
          throw exception
      }
    }

    logger.trace(s"Resolved DB config: ${dbConfig.config}")

    val _ = createDbFileIfDNE()

    dbConfig
  }

  /** The database we are connecting to for our spv node */
  lazy val database: Database = {
    dbConfig.db
  }

  /** The path where our DB is located */
  lazy val dbPath: Path = {
    val pathStr = config.getString("database.dbPath")
    val path = Paths.get(pathStr)
    logger.debug(s"DB path: $path")
    path
  }

  private def createDbFileIfDNE(): Unit = {
    //should add a check in here that we are using sqlite
    if (!Files.exists(dbPath)) {
      logger.debug(s"Creating database directory=$dbPath")
      val _ = Files.createDirectories(dbPath)
      ()
    }
  }

  lazy val chain: ChainParams = {
    val networkStr = config.getString("network")
    networkStr match {
      case "mainnet"  => MainNetChainParams
      case "testnet3" => TestNetChainParams
      case "regtest"  => RegTestNetChainParams
    }
  }

  lazy val network: NetworkParameters = chain.network

  lazy protected val config: Config = {
    val moduleConfig =
      ConfigFactory.load(moduleConfigName)

    // `load` tries to resolve substitions,
    // `parseResources` does not
    val dbConfig = ConfigFactory
      .parseResources("db.conf")

    // loads reference.conf as well as application.conf,
    // if the user has made one
    val unresolvedConfig =
      ConfigFactory
        .load()
        .withFallback(moduleConfig)
        .withFallback(dbConfig)

    logger.trace(s"Unresolved bitcoin-s config:")
    logger.trace(unresolvedConfig.getConfig("bitcoin-s").asReadableJson)

    val withOverrides =
      if (configOverrides.nonEmpty) {
        val overrides =
          configOverrides
          // we reverse to make the configs specified last precedent
          .reverse
            .reduce(_.withFallback(_))

        val interestingOverrides = overrides.getConfig("bitcoin-s")
        logger.debug(s"User-overrides for bitcoin-s config:")
        logger.debug(interestingOverrides.asReadableJson)

        // to make the overrides actually override
        // the default setings we have to do it
        // in this order
        overrides.withFallback(unresolvedConfig)
      } else {
        unresolvedConfig
      }

    val config = withOverrides
      .resolve()
      .getConfig("bitcoin-s")

    logger.debug(s"Resolved bitcoin-s config:")
    logger.debug(config.asReadableJson)

    config
  }

  lazy val datadir: Path = {
    val basedir = Paths.get(config.getString("datadir"))
    val lastDirname = network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
    }
    basedir.resolve(lastDirname)
  }

}
