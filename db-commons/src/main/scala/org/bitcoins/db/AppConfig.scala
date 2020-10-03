package org.bitcoins.db

import java.nio.file.{Files, Path, Paths}

import com.typesafe.config._
import org.bitcoins.core.config._
import org.bitcoins.core.protocol.blockchain.BitcoinChainParams
import org.bitcoins.core.util.{BitcoinSLogger, StartStopAsync}
import slick.basic.DatabaseConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import scala.util.matching.Regex
import scala.util.{Failure, Properties, Success, Try}

/**
  * Everything needed to configure functionality
  * of bitcoin-s applications  is found in here.
  *
  * @see [[https://github.com/bitcoin-s/bitcoin-s-core/blob/master/doc/configuration.md `configuration.md`]]
  *      for more information.
  */
abstract class AppConfig extends StartStopAsync[Unit] with BitcoinSLogger {

  /**
    * Starts this project.
    * After this future resolves, all operations should be
    * able to be performed correctly.
    *
    * Starting may include creating database tables,
    * making directories or files needed later or
    * something else entirely.
    */
  override def start(): Future[Unit]

  /** Releases the thread pool associated with this AppConfig's DB */
  override def stop(): Future[Unit] = {
    Future.successful(slickDbConfig.db.close())
  }

  /** Sub members of AppConfig should override this type with
    * the type of themselves, ensuring `withOverrides` return
    * the correct type
    */
  protected[bitcoins] type ConfigType <: AppConfig

  /** Constructor to make a new instance of this config type */
  protected[bitcoins] def newConfigOfType(
      configOverrides: Seq[Config]): ConfigType

  /** List of user-provided configs that should
    * override defaults
    */
  protected[bitcoins] def configOverrides: List[Config] = List.empty

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

    if (logger.logger.isDebugEnabled()) {
      // force lazy evaluation before we print
      // our lines
      val oldConfStr = this.config.asReadableJson

      logger.debug(s"Creating AppConfig with $numOverrides override(s) ")
      logger.debug(s"Old config:")
      logger.debug(oldConfStr)
    }

    val configOverrides = firstOverride +: configs
    if (logger.logger.isTraceEnabled()) {
      configOverrides.zipWithIndex.foreach {
        case (c, idx) => logger.trace(s"Override no. $idx: ${c.asReadableJson}")
      }
    }
    val newConf = {
      // the idea here is that after resolving the configuration,
      // we extract the value under the 'bitcoin-s' key and use
      // that as our config. here we have to do the reverse, to
      // get the keys to resolve correctly
      val reconstructedStr = s"""
      "bitcoin-s": ${this.config.asReadableJson}
      """
      val reconstructed = ConfigFactory.parseString(reconstructedStr)
      newConfigOfType(reconstructed +: configOverrides)
    }

    // to avoid non-necessary lazy load
    if (logger.logger.isDebugEnabled()) {
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
  protected[bitcoins] def moduleName: String

  /** Chain parameters for the blockchain we're on */
  lazy val chain: BitcoinChainParams = {
    val networkStr = config.getString("network")

    BitcoinNetworks.fromString(networkStr).chainParams
  }

  /** The blockchain network we're on */
  lazy val network: BitcoinNetwork = chain.network

  /**
    * The underlying config that we derive the
    * rest of the fields in this class from
    */
  private[bitcoins] lazy val baseConfig: Config = {
    AppConfig.getBaseConfig(baseDatadir, configOverrides)
  }

  private[bitcoins] lazy val config: Config = {
    val finalConfig = baseConfig.getConfig("bitcoin-s")

    logger.debug(s"Resolved bitcoin-s config:")
    logger.debug(finalConfig.asReadableJson)

    finalConfig
  }

  /** The base data directory. This is where we look for a configuration file */
  protected[bitcoins] def baseDatadir: Path

  /** The network specific data directory. */
  val datadir: Path = {
    val lastDirname = network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
      case SigNet   => "signet"
    }
    baseDatadir.resolve(lastDirname)
  }

  lazy val slickDbConfig: DatabaseConfig[JdbcProfile] = {
    Try {
      DatabaseConfig.forConfig[JdbcProfile](path = moduleName, config = config)
    } match {
      case Success(value) =>
        value
      case Failure(exception) =>
        logger.error(s"Error when loading database from config: $exception")
        logger.error(s"Configuration: ${config.asReadableJson}")
        throw exception
    }
  }
}

object AppConfig extends BitcoinSLogger {

  def getBaseConfig(
      baseDatadir: Path,
      configOverrides: List[Config] = List.empty): Config = {
    val datadirConfig = {
      val file = baseDatadir.resolve("bitcoin-s.conf")
      val config = if (Files.isReadable(file)) {
        ConfigFactory.parseFile(file.toFile)
      } else {
        ConfigFactory.empty()
      }

      val withDatadir =
        ConfigFactory.parseString(s"bitcoin-s.datadir = $baseDatadir")
      withDatadir.withFallback(config)
    }

    // `load` tries to resolve substitutions,
    // `parseResources` does not
    val dbConfig = ConfigFactory
      .parseResources("db.conf")

    // we want to NOT resolve substitutions in the configuration until the user
    // provided configs also has been loaded. .parseResources() does not do that
    // whereas .load() does
    val classPathConfig = {
      val applicationConf = ConfigFactory.parseResources("application.conf")
      val referenceConf = ConfigFactory.parseResources("reference.conf")
      applicationConf.withFallback(referenceConf)
    }

    // we want the data directory configuration
    // to take preference over any bundled (classpath)
    // configurations
    // loads reference.conf (provided by Bitcoin-S)
    val unresolvedConfig = datadirConfig
      .withFallback(classPathConfig)
      .withFallback(dbConfig)

    val withOverrides =
      if (configOverrides.nonEmpty) {
        val overrides =
          configOverrides
            // we reverse to make the configs specified last take precedent
            .reverse
            .reduce(_.withFallback(_))

        // to make the overrides actually override
        // the default settings we have to do it
        // in this order
        overrides.withFallback(unresolvedConfig)
      } else {
        unresolvedConfig
      }

    withOverrides
      .resolve()
  }

  /** The default data directory
    *
    * TODO: use different directories on Windows and Mac,
    * should probably mimic what Bitcoin Core does
    */
  private[bitcoins] val DEFAULT_BITCOIN_S_DATADIR: Path =
    Paths.get(Properties.userHome, ".bitcoin-s")

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
    defaultDatadirRegex.findFirstMatchIn(datadirStr) match {
      case None => () // pass
      case Some(_) =>
        val errMsg =
          List(
            "It looks like you haven't changed the data directory in your test configuration.",
            s"Your data directory is $datadirStr. This would cause tests to potentially",
            "overwrite your existing data, which you probably don't want."
          ).mkString(" ")
        logger.error(errMsg)
        logger.error(s"Configuration: ${config.config.asReadableJson}")
        throw new RuntimeException(errMsg)
    }
  }
}
