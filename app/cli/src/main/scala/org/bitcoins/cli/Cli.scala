package org.bitcoins.cli

import java.net.ConnectException
import java.{util => ju}

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.CliReaders._
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage.{DLCOffer, OracleInfo}
import org.bitcoins.picklers._
import scopt.OParser
import ujson.{Num, Str}
import upickle.{default => up}

case class Config(
    command: CliCommand = CliCommand.NoCommand,
    network: Option[NetworkParameters] = None,
    debug: Boolean = false
)

sealed abstract class CliCommand

object CliCommand {
  case object NoCommand extends CliCommand

  // DLC
  case class CreateDLCOffer(
      amount: Bitcoins,
      oracleInfo: OracleInfo,
      contractInfo: Seq[Sha256DigestBE],
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      escaped: Boolean)
      extends CliCommand

  case class AcceptDLCOffer(offer: DLCOffer, amount: Bitcoins, escaped: Boolean)
      extends CliCommand

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
  case object GetFilterCount extends CliCommand
  case object GetFilterHeaderCount extends CliCommand
  case class Rescan(
      addressBatchSize: Option[Int],
      startBlock: Option[BlockStamp],
      endBlock: Option[BlockStamp],
      force: Boolean)
      extends CliCommand
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
      cmd("getfiltercount")
        .hidden()
        .action((_, conf) => conf.copy(command = GetFilterCount))
        .text(s"Get the number of filters"),
      cmd("getfilterheadercount")
        .hidden()
        .action((_, conf) => conf.copy(command = GetFilterHeaderCount))
        .text(s"Get the number of filter headers"),
      cmd("getbestblockhash")
        .hidden()
        .action((_, conf) => conf.copy(command = GetBestBlockHash))
        .text(s"Get the best block hash"),
      cmd("rescan")
        .hidden()
        .action(
          (_, conf) =>
            conf.copy(
              command = Rescan(addressBatchSize = Option.empty,
                               startBlock = Option.empty,
                               endBlock = Option.empty,
                               force = false)))
        .text(s"Rescan UTXOs")
        .children(
          opt[Unit]("force")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(force = true)
                case other => other
              })),
          opt[Int]("batch-size")
            .optional()
            .action((batchSize, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(addressBatchSize = Option(batchSize))
                case other => other
              })),
          opt[BlockStamp]("start")
            .optional()
            .action((start, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(startBlock = Option(start))
                case other => other
              })),
          opt[BlockStamp]("end")
            .optional()
            .action((end, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(endBlock = Option(end))
                case other => other
              }))
        ),
      cmd("getbalance")
        .hidden()
        .action((_, conf) => conf.copy(command = GetBalance))
        .text("Get the wallet balance"),
      cmd("createdlcoffer")
        .hidden()
        .action(
          (_, conf) =>
            conf.copy(
              command = CreateDLCOffer(0.bitcoins,
                                       null,
                                       Vector.empty,
                                       None,
                                       UInt32.zero,
                                       UInt32.zero,
                                       escaped = false)))
        .text("Creates a DLC offer that another party can accept")
        .children(
          opt[Bitcoins]("amount").required
            .action((amt, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(amount = amt)
                case other => other
              })),
          opt[OracleInfo]("oracleInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(oracleInfo = info)
                case other => other
              })),
          opt[Seq[Sha256DigestBE]]("contractInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(contractInfo = info)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(feeRateOpt = Some(feeRate))
                case other => other
              })),
          opt[UInt32]("locktime")
            .required()
            .action((locktime, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(locktime = locktime)
                case other => other
              })),
          opt[UInt32]("refundlocktime")
            .required()
            .action((refundLT, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(refundLT = refundLT)
                case other => other
              })),
          opt[Boolean]("escaped")
            .action((escaped, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(escaped = escaped)
                case other => other
              }))
        ),
      cmd("acceptdlcoffer")
        .hidden()
        .action((_, conf) =>
          conf.copy(
            command = AcceptDLCOffer(null, 0.bitcoins, escaped = false)))
        .text("Accepts a DLC offer given from another party")
        .children(
          opt[DLCOffer]("offer").required
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(offer = offer)
                case other => other
              })),
          opt[Bitcoins]("amount").required
            .action((amt, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(amount = amt)
                case other => other
              })),
          opt[Boolean]("escaped")
            .action((escaped, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(escaped = escaped)
                case other => other
              }))
        ),
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

  /** Prints the given message to stderr if debug is set */
  def debug(message: Any): Unit = {
    if (config.debug) {
      printerr(s"DEBUG: $message")
    }
  }

  /** Prints the given message to stderr and exist */
  def error(message: String): Nothing = {
    printerr(message)
    // TODO error codes?
    sys.exit(1)
  }

  case class RequestParam(
      method: String,
      params: Seq[ujson.Value.Value] = Nil) {

    lazy val toJsonMap: Map[String, ujson.Value] = {
      Map("method" -> method, "params" -> params)
    }
  }

  val requestParam: RequestParam = config.command match {
    // DLC
    case CreateDLCOffer(amount,
                        oracleInfo,
                        contractInfo,
                        feeRateOpt,
                        locktime,
                        refundLT,
                        escaped) =>
      RequestParam(
        "createdlcoffer",
        Seq(
          up.writeJs(amount),
          up.writeJs(oracleInfo),
          up.writeJs(contractInfo),
          up.writeJs(feeRateOpt.getOrElse(SatoshisPerVirtualByte.one)),
          up.writeJs(locktime),
          up.writeJs(refundLT),
          up.writeJs(escaped)
        )
      )
    case AcceptDLCOffer(offer, amount, escaped) =>
      RequestParam(
        "acceptdlcoffer",
        Seq(up.writeJs(offer), up.writeJs(amount), up.writeJs(escaped)))
    case GetBalance =>
      RequestParam("getbalance")
    case GetNewAddress =>
      RequestParam("getnewaddress")
    case Rescan(addressBatchSize, startBlock, endBlock, force) =>
      RequestParam("rescan",
                   Seq(up.writeJs(addressBatchSize),
                       up.writeJs(startBlock),
                       up.writeJs(endBlock),
                       up.writeJs(force)))

    case SendToAddress(address, bitcoins) =>
      RequestParam("sendtoaddress",
                   Seq(up.writeJs(address), up.writeJs(bitcoins)))
    // height
    case GetBlockCount => RequestParam("getblockcount")
    // filter count
    case GetFilterCount => RequestParam("getfiltercount")
    // filter header count
    case GetFilterHeaderCount => RequestParam("getfilterheadercount")
    // besthash
    case GetBestBlockHash => RequestParam("getbestblockhash")
    // peers
    case GetPeers  => RequestParam("getpeers")
    case NoCommand => ???
  }

  try {

    import com.softwaremill.sttp._
    implicit val backend = HttpURLConnectionBackend()
    val request =
      sttp
        .post(uri"http://$host:$port/")
        .contentType("application/json")
        .body({
          val uuid = ju.UUID.randomUUID.toString
          val paramsWithID: Map[String, ujson.Value] = requestParam.toJsonMap + ("id" -> up
            .writeJs(uuid))
          up.write(paramsWithID)
        })
    debug(s"HTTP request: $request")
    val response = request.send()

    debug(s"HTTP response:")
    debug(response)

    // in order to mimic Bitcoin Core we always send
    // an object looking like {"result": ..., "error": ...}
    val rawBody = response.body match {
      case Left(err)       => err
      case Right(response) => response
    }

    val js = ujson.read(rawBody)
    val jsObj = try {
      js.obj
    } catch {
      case _: Throwable =>
        error(s"Response was not a JSON object! Got: $rawBody")
    }

    /** Gets the given key from jsObj if it exists
      * and is not null */
    def getKey(key: String): Option[ujson.Value] =
      jsObj
        .get(key)
        .flatMap(result => if (result.isNull) None else Some(result))

    /** Converts a `ujson.Value` to String, making an
      * effort to avoid preceding and trailing `"`s */
    def jsValueToString(value: ujson.Value) = value match {
      case Str(string)             => string
      case Num(num) if num.isWhole => num.toLong.toString()
      case Num(num)                => num.toString()
      case rest: ujson.Value       => rest.toString()
    }

    (getKey("result"), getKey("error")) match {
      case (Some(result), None) =>
        val msg = jsValueToString(result)
        println(msg)
      case (None, Some(err)) =>
        val msg = jsValueToString(err)
        error(msg)
      case (None, None) | (Some(_), Some(_)) =>
        error(s"Got unexpected response: $rawBody")
    }
  } catch {
    case _: ConnectException =>
      error(
        "Connection refused! Check that the server is running and configured correctly.")
  }
}
