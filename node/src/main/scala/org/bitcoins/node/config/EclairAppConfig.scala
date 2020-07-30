package org.bitcoins.node.config

import java.nio.file.{Files, Path, Paths}

import com.typesafe.config.Config
import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import org.bitcoins.db._
import org.bitcoins.rpc.client.common.BitcoindVersion

import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  * @param directory The data directory of the wallet
  * @param conf Optional sequence of configuration overrides
  */
case class EclairAppConfig(
    private val directory: Path,
    override val useLogbackConf: Boolean,
    private val conf: Config*)
    extends AppConfig
    with BitcoinSLogger {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList
  override protected[bitcoins] def moduleName: String = "eclair"
  override protected[bitcoins] type ConfigType = EclairAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): EclairAppConfig =
    EclairAppConfig(directory, useLogbackConf, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  lazy val enabled: Boolean = config.getBoolean(s"$moduleName.enabled")

  lazy val eclairDataDir: Path =
    Paths.get(config.getString(s"$moduleName.datadir"))

  lazy val bitcoindDataDir: Path =
    Paths.get(config.getString(s"$moduleName.bitcoind.datadir"))

  lazy val bitcoindVersion: BitcoindVersion = {
    val versionStr = config.getString(s"$moduleName.bitcoind.version")
    val versionOpt = BitcoindVersion.fromString(versionStr)
    versionOpt match {
      case None =>
        throw new RuntimeException(
          s"$versionStr is not a valid bitcoind version")
      case Some(version) =>
        version
    }
  }

  lazy val suredbitsEndpoint: String = config.getStringOrElse(
    "sbclient.endpoint",
    "https://test.api.suredbits.com/dlc/v0")

  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    logger.debug(s"Initializing eclair setup")

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    if (Files.notExists(eclairDataDir)) {
      throw new RuntimeException(
        s"Cannot find eclair data dir at ${eclairDataDir.toString}")
    }

    if (Files.notExists(bitcoindDataDir)) {
      throw new RuntimeException(
        s"Cannot find bitcoind data dir at ${bitcoindDataDir.toString}")
    }

    logger.debug(s"Initializing eclair with bitcoind version $bitcoindVersion")

    FutureUtil.unit
  }

  /** Starts the associated application */
  override def start(): Future[Unit] = FutureUtil.unit
}

object EclairAppConfig {

  /** Constructs a eclair configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(
      useLogbackConf: Boolean,
      confs: Config*): EclairAppConfig =
    EclairAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR,
                    useLogbackConf,
                    confs: _*)
}
