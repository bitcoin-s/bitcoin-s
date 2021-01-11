package org.bitcoins.server.routes

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config._
import org.bitcoins.core.util.{BitcoinSLogger, EnvUtil}
import org.bitcoins.db.AppConfig
import org.bitcoins.db.AppConfig.safePathToString

import java.nio.file.{Path, Paths}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Properties

trait BitcoinSRunner extends BitcoinSLogger {

  protected def args: Array[String]

  def actorSystemName: String

  implicit lazy val system: ActorSystem = {
    val system = ActorSystem(actorSystemName, baseConfig)
    system.log.info("Akka logger started")
    system
  }
  implicit lazy val ec: ExecutionContext = system.dispatcher

  lazy val argsWithIndex: Vector[(String, Int)] = args.toVector.zipWithIndex

  /** The ip address we are binding the server to */
  lazy val rpcBindOpt: Option[String] = {
    val rpcbindOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcbind")
    rpcbindOpt.map {
      case (_, idx) =>
        args(idx + 1)
    }
  }

  lazy val rpcPortOpt: Option[Int] = {
    val portOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcport")
    portOpt.map {
      case (_, idx) => args(idx + 1).toInt
    }
  }

  lazy val forceChainWorkRecalc: Boolean =
    args.exists(_.toLowerCase == "--force-recalc-chainwork")

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

  lazy val baseConfig: Config = configIndexOpt match {
    case None =>
      AppConfig
        .getBaseConfig(datadirPath)
        .withFallback(datadirConfig)
        .resolve()
    case Some(configIndex) =>
      val str = args(configIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      val path = Paths.get(usableStr)
      ConfigFactory
        .parseFile(path.toFile)
        .withFallback(datadirConfig)
        .resolve()
  }

  /** Base directory for all bitcoin-s data. This is the resulting datadir from
    * the --datadir option and all configuration files.
    */
  lazy val datadir: Path =
    Paths.get(baseConfig.getString("bitcoin-s.datadir"))

  def startup: Future[Unit]

  // start everything!
  final def run(): Unit = {

    /** Directory specific for current network */
    val networkDatadir: Path = {
      val networkStr: String =
        baseConfig.getString("bitcoin-s.network")

      val network: BitcoinNetwork = networkStr.toLowerCase match {
        case "mainnet"  => MainNet
        case "main"     => MainNet
        case "testnet3" => TestNet3
        case "testnet"  => TestNet3
        case "test"     => TestNet3
        case "regtest"  => RegTest
        case "signet"   => SigNet
        case "sig"      => SigNet
        case _: String =>
          throw new IllegalArgumentException(s"Invalid network $networkStr")
      }

      val lastDirname = network match {
        case MainNet  => "mainnet"
        case TestNet3 => "testnet3"
        case RegTest  => "regtest"
        case SigNet   => "signet"
      }
      datadir.resolve(lastDirname)
    }

    //We need to set the system property before any logger instances
    //are in instantiated. If we don't do this, we will not log to
    //the correct location
    //see: https://github.com/bitcoin-s/bitcoin-s/issues/2496
    System.setProperty("bitcoins.log.location",
                       networkDatadir.toAbsolutePath.toString)

    logger.info(s"version=${EnvUtil.getVersion}")

    logger.info(s"networkDatadir=${networkDatadir.toAbsolutePath.toString}")
    val runner = startup
    runner.failed.foreach { err =>
      logger.error(s"Failed to startup server!", err)
      sys.exit(1)
    }(scala.concurrent.ExecutionContext.Implicits.global)
  }
}
