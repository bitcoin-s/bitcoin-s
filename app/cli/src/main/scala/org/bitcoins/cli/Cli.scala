package org.bitcoins.cli

import org.bitcoins.picklers._

import scopt.OParser
import org.bitcoins.core.config.NetworkParameters

import upickle.{default => up}

import CliReaders._
import org.bitcoins.core.protocol._
import org.bitcoins.core.currency._
import org.bitcoins.cli.CliCommand.GetBalance
import org.bitcoins.cli.CliCommand.GetNewAddress
import org.bitcoins.cli.CliCommand.SendToAddress
import org.bitcoins.cli.CliCommand.GetBlockCount
import org.bitcoins.cli.CliCommand.GetBestBlockHash
import org.bitcoins.cli.CliCommand.GetPeers
import org.bitcoins.cli.CliCommand.NoCommand
import java.net.ConnectException
import java.{util => ju}

case class Config(
    command: CliCommand = CliCommand.NoCommand,
    network: Option[NetworkParameters] = None,
    debug: Boolean = false
)

sealed abstract class CliCommand

object CliCommand {
  case object NoCommand extends CliCommand

  // Wallet
  case class SendToAddress(destination: BitcoinAddress, amount: Bitcoins)
      extends CliCommand
  case object GetNewAddress extends CliCommand
  case object GetBalance extends CliCommand

  // Node
  case object GetPeers extends CliCommand

  // Chain
  case object GetBestBlockHash extends CliCommand
  case object GetBlockCount extends CliCommand
}

object Cli extends App {

  val builder = OParser.builder[Config]

  val parser = {
    import CliCommand._
    import builder._
    OParser.sequence(
      programName("bitcoin-s-cli"),
      opt[NetworkParameters]('n', "network")
        .action((np, conf) => conf.copy(network = Some(np)))
        .text("Select the active network."),
      opt[Unit]("debug")
        .action((_, conf) => conf.copy(debug = true))
        .text("Print debugging information"),
      cmd("getblockcount")
        .hidden()
        .action((_, conf) => conf.copy(command = GetBlockCount))
        .text(s"Get the block height"),
      cmd("getbestblockhash")
        .hidden()
        .action((_, conf) => conf.copy(command = GetBestBlockHash))
        .text(s"Get the best block hash"),
      cmd("getbalance")
        .hidden()
        .action((_, conf) => conf.copy(command = GetBalance))
        .text("Get the wallet balance"),
      cmd("getnewaddress")
        .hidden()
        .action((_, conf) => conf.copy(command = GetNewAddress))
        .text("Get a new address"),
      cmd("sendtoaddress")
        .hidden()
        .action(
          // TODO how to handle null here?
          (_, conf) => conf.copy(command = SendToAddress(null, 0.bitcoin)))
        .text("Send money to the given address")
        .children(
          opt[BitcoinAddress]("address")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(destination = addr)
                case other => other
              })),
          opt[Bitcoins]("amount")
            .required()
            .action((btc, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(amount = btc)
                case other => other
              }))
        ),
      cmd("getpeers")
        .hidden()
        .action((_, conf) => conf.copy(command = GetPeers))
        .text(s"List the connected peers"),
      help('h', "help").text("Display this help message and exit"),
      arg[String]("<cmd>")
        .optional()
        .text(
          "The command and arguments to be executed. Try bitcoin-s-cli help for a list of all commands"),
      checkConfig {
        case Config(NoCommand, _, _) =>
          failure("You need to provide a command!")
        case _ => success
      }
    )
  }

  // TODO make this dynamic
  val port = 9999
  val host = "localhost"

  val config: Config = OParser.parse(parser, args, Config()) match {
    case None       => sys.exit(1)
    case Some(conf) => conf
  }

  import System.err.{println => printerr}

  /** Prints the given message to stderr */
  def debug(message: Any): Unit = {
    if (config.debug) {
      printerr(s"DEBUG: $message")
    }
  }

  case class RequestParam(method: String, params: Seq[ujson.Value] = Nil) {

    lazy val toJsonMap: Map[String, ujson.Value] = {
      Map("method" -> method, "params" -> params)
    }
  }

  val requestParam: RequestParam = config.command match {
    case GetBalance =>
      RequestParam("getbalance")
    case GetNewAddress =>
      RequestParam("getnewaddress")

    case SendToAddress(address, bitcoins) =>
      RequestParam("sendtoaddress", Seq(up.write(address), up.write(bitcoins)))
    // height
    case GetBlockCount => RequestParam("getblockcount")
    // besthash
    case GetBestBlockHash => RequestParam("getbestblockhash")
    // peers
    case GetPeers  => RequestParam("getpeers")
    case NoCommand => ???
  }

  try {

    import com.softwaremill.sttp._
    implicit val backend = HttpURLConnectionBackend()
    // TODO sttp URI provides a bunch of handy features, use these properly
    val request =
      sttp
        .post(uri"http://$host:$port/")
        .contentType("application/json")
        .body({
          val uuid = ju.UUID.randomUUID.toString
          val paramsWithID: Map[String, ujson.Value] = requestParam.toJsonMap + ("id" -> up
            .write(uuid))
          up.write(paramsWithID)
        })
    debug(s"HTTP request: $request")
    val response = request.send()
    // TODO check status code, handle errors, etc
    debug(s"HTTP response:")
    debug(response)
    response.body match {
      case Left(err) =>
        printerr("Error in response body:")
        printerr(err)
      case Right(response) => println(response)
    }
  } catch {
    case _: ConnectException =>
      print(
        "Connection refused! Check that the server is running and configured correctly.")
      // TODO defined error codes?
      sys.exit(1)
  }
}
