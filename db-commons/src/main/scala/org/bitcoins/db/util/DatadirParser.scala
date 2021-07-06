package org.bitcoins.db.util

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.db.AppConfig
import org.bitcoins.db.AppConfig.safePathToString

import java.nio.file.{Path, Paths}

/** Parses the correct datadir given the possible input sources for datadir config
  * 1. The --datadir command line flag
  * 2. Inferring the datadir based on the bitcoin network configured
  * 3. ??? Anything else i'm forgetting ????
  */
case class DatadirParser(
    serverArgs: ServerArgParser,
    customFinalDirOpt: Option[String]) {

  /** Sets the default data dir, overridden by the --datadir option */
  private lazy val datadirPath: Path = serverArgs.datadirOpt match {
    case None          => AppConfig.DEFAULT_BITCOIN_S_DATADIR
    case Some(datadir) => datadir
  }

  lazy val datadirConfig: Config =
    ConfigFactory.parseString(
      s"bitcoin-s.datadir = ${safePathToString(datadirPath)}")

  lazy val networkConfig: Config = serverArgs.networkOpt match {
    case Some(network) =>
      val networkStr = DatadirUtil.networkStrToDirName(network.name)
      ConfigFactory.parseString(s"bitcoin-s.network = $networkStr")
    case None => ConfigFactory.empty()
  }

  lazy val baseConfig: Config = {
    serverArgs.configOpt match {
      case None =>
        AppConfig
          .getBaseConfig(datadirPath, List(networkConfig))
          .withFallback(datadirConfig)
          .resolve()
      case Some(config) =>
        val conf = ConfigFactory
          .parseFile(config.toFile)
          .withFallback(datadirConfig)
        networkConfig.withFallback(conf)
    }
  }

  /** Base directory for all bitcoin-s data. This is the resulting datadir from
    * the --datadir option and all configuration files.
    */
  lazy val datadir: Path =
    Paths.get(baseConfig.getString("bitcoin-s.datadir"))

  /** Directory specific for current network or custom dir */
  def usedDir: Path =
    DatadirUtil.getFinalDatadir(datadir, baseConfig, customFinalDirOpt)
}
