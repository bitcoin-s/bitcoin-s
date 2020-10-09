package org.bitcoins.server

import java.nio.file.{Path, Paths}

import akka.actor.ActorSystem
import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.core.config._
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.{AppConfig, ConfigOps}

import scala.concurrent.ExecutionContext
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

  private lazy val datadirPathOpt: Option[Path] = dataDirIndexOpt match {
    case None => None
    case Some((_, dataDirIndex)) =>
      val str = args(dataDirIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      Some(Paths.get(usableStr))
  }

  private lazy val configIndexOpt: Option[(String, Int)] = {
    argsWithIndex.find(_._1.toLowerCase == "--conf")
  }

  lazy val baseConfig: Config = configIndexOpt match {
    case None =>
      val configPath =
        datadirPathOpt.getOrElse(AppConfig.DEFAULT_BITCOIN_S_DATADIR)
      AppConfig
        .getBaseConfig(configPath)
        .resolve()
    case Some((_, configIndex)) =>
      val str = args(configIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      val path = Paths.get(usableStr)
      ConfigFactory
        .parseFile(path.toFile)
        .resolve()
  }

  lazy val configDataDir: Path = Paths.get(
    baseConfig.getStringOrElse("bitcoin-s.datadir",
                               AppConfig.DEFAULT_BITCOIN_S_DATADIR.toString))

  lazy val datadirPath: Path = datadirPathOpt.getOrElse(configDataDir)

  private lazy val networkStr: String =
    baseConfig.getString("bitcoin-s.network")

  private lazy val network: BitcoinNetwork =
    BitcoinNetworks.fromString(networkStr)

  lazy val datadir: Path = {
    lazy val lastDirname = network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
      case SigNet   => "signet"
    }
    val dir = datadirPath.resolve(lastDirname)

    System.setProperty("bitcoins.log.location", dir.toAbsolutePath.toString)
    dir
  }
}
