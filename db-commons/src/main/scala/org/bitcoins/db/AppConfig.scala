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

import scala.util.Properties
import scala.util.matching.Regex
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/**
  * Everything needed to configure functionality
  * of bitcoin-s applications  is found in here.
  *
  * @see [[https://github.com/bitcoin-s/bitcoin-s-core/blob/master/doc/configuration.md `configuration.md`]]
  *      for more information.
  */
abstract class AppConfig extends BitcoinSLogger {

  /**
    * Initializes this project.
    * After this future resolves, all operations should be
    * able to be performed correctly.
    *
    * Initializing may include creating database tables,
    * making directories or files needed latern or
    * something else entirely.
    */
  def initialize()(implicit ec: ExecutionContext): Future[Unit]

  /** Sub members of AppConfig should override this type with
    * the type of themselves, ensuring `withOverrides` return
    * the correct type
    */
  protected type ConfigType <: AppConfig

  /** Constructor to make a new instance of this config type */
  protected def newConfigOfType(configOverrides: List[Config]): ConfigType

  /** List of user-provided configs that should
    * override defaults
    */
  protected val configOverrides: List[Config] = List.empty

  /**
    * This method returns a new `AppConfig`, where every
    * key under `bitcoin-s` overrides the configuration
    * picked up by other means (the `reference.conf`
    * provided by bitcoin-s and the `application.conf`
    * provided by the user). If you pass in configs with
    * overlapping keys (e.g. several configs with the key
    * `bitcoin-s.network`), the latter config overrides the
    * first.
    */
  def withOverrides(config: Config, configs: Config*): ConfigType = {
    // the two val assignments below are workarounds
    // for awkward name resolution in the block below
    val firstOverride = config

    val numOverrides = configs.length + 1

    if (logger.isDebugEnabled()) {
      // force lazy evaluation before we print
      // our lines
      val oldConfStr = this.config.asReadableJson

      logger.debug(s"Creating AppConfig with $numOverrides override(s) ")
      logger.debug(s"Old config:")
      logger.debug(oldConfStr)
    }

    val newConf = newConfigOfType(
      configOverrides = List(firstOverride) ++ configs
    )

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
    * Name of the module. `chain`, `wallet`, `node` etc.
    */
  protected def moduleName: String

  /**
    * The configuration details for connecting/using the database for our projects
    * that require datbase connections
    */
  lazy val dbConfig: DatabaseConfig[SQLiteProfile] = {
    val dbConfig = {
      Try {
        DatabaseConfig.forConfig[SQLiteProfile](path = moduleName, config)
      } match {
        case Success(value) =>
          value
        case Failure(exception) =>
          logger.error(s"Error when loading database from config: $exception")
          logger.error(s"Configuration: ${config.asReadableJson}")
          throw exception
      }
    }

    logger.debug(s"Resolved DB config: ${dbConfig.config}")

    val _ = createDbFileIfDNE()

    dbConfig
  }

  /** The database we are connecting to */
  lazy val database: Database = {
    dbConfig.db
  }

  /** The path where our DB is located */
  // todo: what happens to this if we
  // dont use SQLite?
  lazy val dbPath: Path = {
    val pathStr = config.getString(s"$moduleName.db.path")
    val path = Paths.get(pathStr)
    logger.debug(s"DB path: $path")
    path
  }

  /** The name of our database */
  // todo: what happens to this if we
  // dont use SQLite?
  lazy val dbName: String = {
    config.getString(s"$moduleName.db.name")
  }

  private def createDbFileIfDNE(): Unit = {
    //should add a check in here that we are using sqlite
    if (!Files.exists(dbPath)) {
      val _ = {
        logger.debug(s"Creating database directory=$dbPath")
        Files.createDirectories(dbPath)
        val dbFilePath = dbPath.resolve(dbName)
        logger.debug(s"Creating database file=$dbFilePath")
        Files.createFile(dbFilePath)
      }

      ()
    }
  }

  /** Chain parameters for the blockchain we're on */
  lazy val chain: ChainParams = {
    val networkStr = config.getString("network")
    networkStr match {
      case "mainnet"  => MainNetChainParams
      case "testnet3" => TestNetChainParams
      case "regtest"  => RegTestNetChainParams
      case other: String =>
        throw new IllegalArgumentException(
          s"'$other' is not a recognized network! Available options: mainnet, testnet3, regtest")
    }
  }

  /** The blockchain network we're on */
  lazy val network: NetworkParameters = chain.network

  /**
    * The underlying config that we derive the
    * rest of the fields in this class from
    */
  private[bitcoins] lazy val config: Config = {
    // `load` tries to resolve substitions,
    // `parseResources` does not
    val dbConfig = ConfigFactory
      .parseResources("db.conf")

    logger.trace(
      s"DB config: ${dbConfig.getConfig("bitcoin-s").asReadableJson}")

    // we want to NOT resolve substitutions in the configuraton until the user
    // provided configs also has been loaded. .parseResources() does not do that
    // whereas .load() does
    val classPathConfig = {
      val applicationConf = ConfigFactory.parseResources("application.conf")
      val referenceConf = ConfigFactory.parseResources("reference.conf")
      applicationConf.withFallback(referenceConf)
    }

    logger.trace(
      s"Classpath config: ${classPathConfig.getConfig("bitcoin-s").asReadableJson}")

    // loads reference.conf as well as application.conf,
    // if the user has made one
    val unresolvedConfig = classPathConfig
      .withFallback(dbConfig)

    logger.trace(s"Unresolved bitcoin-s config:")
    logger.trace(unresolvedConfig.getConfig("bitcoin-s").asReadableJson)

    val withOverrides =
      if (configOverrides.nonEmpty) {
        val overrides =
          configOverrides
          // we reverse to make the configs specified last take precedent
          .reverse
            .reduce(_.withFallback(_))

        val interestingOverrides = overrides.getConfig("bitcoin-s")
        logger.trace(s"User-overrides for bitcoin-s config:")
        logger.trace(interestingOverrides.asReadableJson)

        // to make the overrides actually override
        // the default setings we have to do it
        // in this order
        overrides.withFallback(unresolvedConfig)
      } else {
        logger.trace(s"No user-provided overrides")
        unresolvedConfig
      }

    val config = withOverrides
      .resolve()
      .getConfig("bitcoin-s")

    logger.debug(s"Resolved bitcoin-s config:")
    logger.debug(config.asReadableJson)

    config

  }

  /** The data directory used by bitcoin-s apps */
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

object AppConfig extends BitcoinSLogger {

  /**
    * Matches the default data directory location
    * with a network appended,
    * both with and without a trailing `/`
    */
  private val defaultDatadirRegex: Regex = {
    (Properties.userHome + "/.bitcoin-s/(testnet3|mainnet|regtest)/?$").r
  }

  /**
    * Throws if the encountered datadir is the default one. Useful
    * in tests, to make sure you don't blow up important data.
    */
  private[bitcoins] def throwIfDefaultDatadir(config: AppConfig): Unit = {
    val datadirStr = config.datadir.toString()
    AppConfig.defaultDatadirRegex.findFirstMatchIn(datadirStr) match {
      case None => () // pass
      case Some(_) =>
        val errMsg =
          List(
            "It looks like you haven't changed the data directory in your test configuration.",
            s"Your data directory is $datadirStr. This would cause tests to potentially",
            "overwrite your existing data, which you probably don't want."
          ).mkString(" ")
        throw new RuntimeException(errMsg)
    }
  }
}
