package org.bitcoins.db.util

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.db.AppConfig
import org.bitcoins.db.AppConfig.safePathToString

import java.nio.file.{Path, Paths}
import scala.util.Properties

case class DatadirParser(
    args: Vector[String],
    networkOpt: Option[BitcoinNetwork],
    customFinalDirOpt: Option[String]) {

  private val argsWithIndex: Vector[(String, Int)] = args.zipWithIndex

  private lazy val dataDirIndexOpt: Option[(String, Int)] = {
    argsWithIndex.find(_._1.toLowerCase == "--datadir")
  }

  /** Sets the default data dir, overridden by the --datadir option */
  private lazy val datadirPath: Path = dataDirIndexOpt match {
    case None => AppConfig.DEFAULT_BITCOIN_S_DATADIR
    case Some((_, dataDirIndex)) =>
      val str = args(dataDirIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      Paths.get(usableStr)
  }

  private lazy val configIndexOpt: Option[Int] = {
    argsWithIndex.find(_._1.toLowerCase == "--conf").map(_._2)
  }

  lazy val datadirConfig: Config =
    ConfigFactory.parseString(
      s"bitcoin-s.datadir = ${safePathToString(datadirPath)}")

  lazy val networkConfig: Config = networkOpt match {
    case Some(network) =>
      val networkStr = DatadirUtil.networkStrToDirName(network.name)
      ConfigFactory.parseString(s"bitcoin-s.network = $networkStr")
    case None => ConfigFactory.empty()
  }

  lazy val baseConfig: Config = configIndexOpt match {
    case None =>
      AppConfig
        .getBaseConfig(datadirPath, List(networkConfig))
        .withFallback(datadirConfig)
        .resolve()
    case Some(configIndex) =>
      val str = args(configIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      val path = Paths.get(usableStr)
      val conf = ConfigFactory
        .parseFile(path.toFile)
        .withFallback(datadirConfig)
      networkConfig.withFallback(conf)
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
