package org.bitcoins.server.util

import akka.actor.ActorSystem
import org.bitcoins.core.config._

trait BitcoinSApp {
  def actorSystemName: String

  implicit lazy val system: ActorSystem = ActorSystem(actorSystemName)

  def commandLineArgs: Array[String]

  lazy val argsWithIndex: Vector[(String, Int)] =
    commandLineArgs.toVector.zipWithIndex

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

  lazy val forceChainWorkRecalc: Boolean =
    commandLineArgs.exists(_.toLowerCase == "--force-recalc-chainwork")

  /** Useful for projects like the oracle server to specify a custom directory inside of ~./bitcoin-s */
  def customFinalDirOpt: Option[String]
}

/** Trait for using BitcoinS app with a daemon backend */
trait BitcoinSAppScalaDaemon extends App with BitcoinSApp {
  final override def commandLineArgs: Array[String] = args
}
