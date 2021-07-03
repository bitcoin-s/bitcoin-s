package org.bitcoins.db.util

import org.bitcoins.core.config._

import java.nio.file.{Path, Paths}
import scala.util.Properties

/** Parses arguments passed to the bitcoin-s app server as command line arguments
  * This does NOT consider things that exist in reference.conf or application.conf files
  */
case class ServerArgParser(commandLineArgs: Vector[String]) {

  private lazy val argsWithIndex: Vector[(String, Int)] =
    commandLineArgs.zipWithIndex

  /** The ip address we are binding the server to */
  lazy val rpcBindOpt: Option[String] = {
    val rpcbindOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcbind")
    rpcbindOpt.map { case (_, idx) =>
      commandLineArgs(idx + 1)
    }
  }

  lazy val rpcPortOpt: Option[Int] = {
    val portOpt = argsWithIndex.find(_._1.toLowerCase == "--rpcport")
    portOpt.map { case (_, idx) =>
      commandLineArgs(idx + 1).toInt
    }
  }

  lazy val networkOpt: Option[BitcoinNetwork] = {
    val netOpt = argsWithIndex.find(_._1.toLowerCase == "--network")
    netOpt.map { case (_, idx) =>
      val string = commandLineArgs(idx + 1)
      string.toLowerCase match {
        case "mainnet"  => MainNet
        case "main"     => MainNet
        case "testnet3" => TestNet3
        case "testnet"  => TestNet3
        case "test"     => TestNet3
        case "regtest"  => RegTest
        case "signet"   => SigNet
        case "sig"      => SigNet
        case _: String =>
          throw new IllegalArgumentException(s"Invalid network $string")
      }
    }
  }

  private lazy val dataDirIndexOpt: Option[(String, Int)] = {
    argsWithIndex.find(_._1.toLowerCase == "--datadir")
  }

  /** The datadir passed in as a command line arg using --datadir */
  lazy val datadirOpt: Option[Path] = dataDirIndexOpt.map { case (_, idx) =>
    val str = commandLineArgs(idx + 1)
    val usableStr = str.replace("~", Properties.userHome)
    Paths.get(usableStr)
  }

  private lazy val configIndexOpt: Option[Int] = {
    argsWithIndex.find(_._1.toLowerCase == "--conf").map(_._2)
  }

  /** A custom configuration file passed in as a command line arg with --conf */
  lazy val configOpt: Option[Path] = configIndexOpt.map { idx =>
    val str = commandLineArgs(idx + 1)
    val usableStr = str.replace("~", Properties.userHome)
    Paths.get(usableStr)
  }
}
