package org.bitcoins.oracle.server

import java.nio.file.{Path, Paths}

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config._
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db._
import org.bitcoins.dlc.oracle.DLCOracleAppConfig
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.server.Server

import scala.concurrent.ExecutionContext
import scala.util.Properties

object Main extends App with BitcoinSLogger {

  val argsWithIndex = args.zipWithIndex

  val dataDirIndexOpt = {
    argsWithIndex.find(_._1.toLowerCase == "--datadir")
  }

  val datadirPathOpt = dataDirIndexOpt match {
    case None => None
    case Some((_, dataDirIndex)) =>
      val str = args(dataDirIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      Some(Paths.get(usableStr))
  }

  val configIndexOpt = {
    argsWithIndex.find(_._1.toLowerCase == "--conf")
  }

  val baseConfig = configIndexOpt match {
    case None =>
      val configPath =
        datadirPathOpt.getOrElse(AppConfig.DEFAULT_BITCOIN_S_DATADIR)
      AppConfig.getBaseConfig(configPath)
    case Some((_, configIndex)) =>
      val str = args(configIndex + 1)
      val usableStr = str.replace("~", Properties.userHome)
      val path = Paths.get(usableStr)
      ConfigFactory.parseFile(path.toFile).resolve()
  }

  val configDataDir = Paths.get(
    baseConfig.getStringOrElse("bitcoin-s.datadir",
                               AppConfig.DEFAULT_BITCOIN_S_DATADIR.toString))
  val datadirPath = datadirPathOpt.getOrElse(configDataDir)

  val networkStr = baseConfig.getString("bitcoin-s.network")
  val network = BitcoinNetworks.fromString(networkStr)

  val datadir: Path = {
    val lastDirname = network match {
      case MainNet  => "mainnet"
      case TestNet3 => "testnet3"
      case RegTest  => "regtest"
      case SigNet   => "signet"
    }
    datadirPath.resolve(lastDirname)
  }

  val rpcPortOpt: Option[Int] = {
    val portOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcport")
    portOpt.map {
      case (_, idx) => args(idx + 1).toInt
    }
  }

  System.setProperty("bitcoins.log.location", datadir.toAbsolutePath.toString)

  implicit val system: ActorSystem = ActorSystem("bitcoin-s", baseConfig)
  implicit val ec: ExecutionContext = system.dispatcher

  system.log.info("Akka logger started")

  implicit val conf: DLCOracleAppConfig = {
    val dataDirOverrideOpt = datadirPathOpt.map(dir =>
      ConfigFactory.parseString(s"bitcoin-s.datadir = $dir"))

    dataDirOverrideOpt match {
      case Some(dataDirOverride) =>
        DLCOracleAppConfig(datadirPath, baseConfig, dataDirOverride)
      case None =>
        DLCOracleAppConfig(datadirPath, baseConfig)
    }
  }

  // TODO need to prompt user for these
  val bip39PasswordOpt = None
  val aesPassword = BIP39KeyManager.badPassphrase

  for {
    _ <- conf.start()
    oracle <- conf.initialize(aesPassword, bip39PasswordOpt)

    routes = Seq(OracleRoutes(oracle))
    server = rpcPortOpt match {
      case Some(rpcport) =>
        Server(conf, routes, rpcport = rpcport)
      case None =>
        conf.rpcPortOpt match {
          case Some(rpcport) =>
            Server(conf, routes, rpcport)
          case None =>
            Server(conf, routes)
        }
    }

    _ <- server.start()
  } yield {
    logger.info(s"Done starting oracle!")
    sys.addShutdownHook {
      logger.error(s"Exiting process")

      conf.stop().foreach(_ => logger.info(s"Stopped DLC Oracle"))
      system.terminate().foreach(_ => logger.info(s"Actor system terminated"))
    }
  }
}
