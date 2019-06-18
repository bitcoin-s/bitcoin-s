package org.bitcoins.cli

import scopt.OParser
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.config.Networks
import org.bitcoins.core.config.RegTest

case class Config(
    height: Boolean = false,
    bestHash: Boolean = false,
    network: NetworkParameters = RegTest
)

object Cli extends App {

  val builder = OParser.builder[Config]

  val parser = {
    implicit val npReads: scopt.Read[NetworkParameters] =
      new scopt.Read[NetworkParameters] {
        val arity: Int = 1
        val reads: String => NetworkParameters = str =>
          Networks.knownNetworks
            .find(_.toString.toLowerCase == str.toLowerCase)
            .getOrElse {
              val networks =
                Networks.knownNetworks
                  .map(_.toString.toLowerCase)
                  .mkString(", ")
              val msg =
                s"$str is not a valid network! Valid networks: $networks"
              sys.error(msg)
          }
      }
    import builder._
    OParser.sequence(
      programName("bitcoin-s-cli"),
      cmd("height")
        .action((_, conf) => conf.copy(height = true))
        .text(s"Get the block height"),
      cmd("best-hash")
        .action((_, conf) => conf.copy(bestHash = true))
        .text(s"Get the best block hash"),
      opt[NetworkParameters]('n', "network")
        .required()
        .action((np, conf) => conf.copy(network = np))
        .text("Select the active network"),
      help('h', "help").text("Display this help message and exit"),
      checkConfig { conf =>
        if (!(conf.bestHash || conf.height)) failure("No command given!")
        else success
      }
    )
  }

  OParser.parse(parser, args, Config()) match {
    case None => sys.exit(1)
    case Some(value) =>
      println(s"Parsed args successfully! Result: $value")
      sys.exit()
  }
}
