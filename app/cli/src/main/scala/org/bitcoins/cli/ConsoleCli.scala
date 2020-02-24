package org.bitcoins.cli

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.CliReaders._
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{EmptyTransaction, Transaction}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage._
import org.bitcoins.picklers._
import scopt.OParser
import ujson.{Num, Str}
import upickle.{default => up}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object ConsoleCli {

  def parser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]

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
      cmd("createdlcoffer")
        .hidden()
        .action(
          (_, conf) =>
            conf.copy(
              command = CreateDLCOffer(OracleInfo.empty,
                                       ContractInfo.empty,
                                       Satoshis.zero,
                                       None,
                                       UInt32.zero,
                                       UInt32.zero,
                                       escaped = false)))
        .text("Creates a DLC offer that another party can accept")
        .children(
          opt[OracleInfo]("oracleInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(oracleInfo = info)
                case other => other
              })),
          opt[ContractInfo]("contractInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(contractInfo = info)
                case other => other
              })),
          opt[Satoshis]("collateral")
            .required()
            .action((collateral, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(collateral = collateral)
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
          opt[Unit]("escaped")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case create: CreateDLCOffer =>
                  create.copy(escaped = true)
                case other => other
              }))
        ),
      cmd("acceptdlcoffer")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = AcceptDLCOffer(null, escaped = false)))
        .text("Accepts a DLC offer given from another party")
        .children(
          opt[DLCOffer]("offer").required
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(offer = offer)
                case other => other
              })),
          opt[Unit]("escaped")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(escaped = true)
                case other => other
              }))
        ),
      cmd("signdlc")
        .hidden()
        .action(
          (_, conf) => conf.copy(command = SignDLC(null, escaped = false)))
        .text("Signs a DLC")
        .children(
          opt[DLCAccept]("accept").required
            .action((accept, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(accept = accept)
                case other => other
              })),
          opt[Unit]("escaped")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(escaped = true)
                case other => other
              }))
        ),
      cmd("adddlcsigs")
        .hidden()
        .action((_, conf) => conf.copy(command = AddDLCSigs(null)))
        .text("Adds DLC Signatures into the database")
        .children(
          opt[DLCSign]("sigs").required
            .action((sigs, conf) =>
              conf.copy(command = conf.command match {
                case addDLCSigs: AddDLCSigs =>
                  addDLCSigs.copy(sigs = sigs)
                case other => other
              }))
        ),
      cmd("initdlcmutualclose")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = InitDLCMutualClose(null, null, escaped = false)))
        .text("Sign Mutual Close Tx for given oracle event")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case initClose: InitDLCMutualClose =>
                  initClose.copy(eventId = eventId)
                case other => other
              })),
          opt[SchnorrDigitalSignature]("oraclesig").required
            .action((sig, conf) =>
              conf.copy(command = conf.command match {
                case initClose: InitDLCMutualClose =>
                  initClose.copy(oracleSig = sig)
                case other => other
              })),
          opt[Unit]("escaped")
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case initClose: InitDLCMutualClose =>
                  initClose.copy(escaped = true)
                case other => other
              }))
        ),
      cmd("acceptdlcmutualclose")
        .hidden()
        .action((_, conf) => conf.copy(command = AcceptDLCMutualClose(null)))
        .text("Sign Mutual Close Tx for given oracle event")
        .children(
          opt[DLCMutualCloseSig]("closesig").required
            .action((closeSig, conf) =>
              conf.copy(command = conf.command match {
                case acceptClose: AcceptDLCMutualClose =>
                  acceptClose.copy(mutualCloseSig = closeSig)
                case other => other
              }))
        ),
      cmd("getdlcfundingtx")
        .hidden()
        .action((_, conf) => conf.copy(command = GetDLCFundingTx(null)))
        .text("Returns the Funding Tx corresponding to the DLC with the given eventId")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case getDLCFundingTx: GetDLCFundingTx =>
                  getDLCFundingTx.copy(eventId = eventId)
                case other => other
              }))
        ),
      cmd("executedlcunilateralclose")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = ExecuteDLCUnilateralClose(null, null)))
        .text("Executes a force close for the DLC with the given eventId")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCUnilateralClose: ExecuteDLCUnilateralClose =>
                  executeDLCUnilateralClose.copy(eventId = eventId)
                case other => other
              })),
          opt[SchnorrDigitalSignature]("oraclesig").required
            .action((sig, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCUnilateralClose: ExecuteDLCUnilateralClose =>
                  executeDLCUnilateralClose.copy(oracleSig = sig)
                case other => other
              }))
        ),
      cmd("executedlcforceclose")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = ExecuteDLCForceClose(null, null)))
        .text("Executes a force close for the DLC with the given eventId")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCForceClose: ExecuteDLCForceClose =>
                  executeDLCForceClose.copy(eventId = eventId)
                case other => other
              })),
          opt[SchnorrDigitalSignature]("oraclesig").required
            .action((sig, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCForceClose: ExecuteDLCForceClose =>
                  executeDLCForceClose.copy(oracleSig = sig)
                case other => other
              }))
        ),
      cmd("claimdlcremotefunds")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = ClaimDLCRemoteFunds(null, EmptyTransaction)))
        .text("Claims the remote funds for the corresponding DLC")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCRemoteFunds: ClaimDLCRemoteFunds =>
                  claimDLCRemoteFunds.copy(eventId = eventId)
                case other => other
              })),
          opt[Transaction]("forceclosetx")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCRemoteFunds: ClaimDLCRemoteFunds =>
                  claimDLCRemoteFunds.copy(forceCloseTx = tx)
                case other => other
              }))
        ),
      cmd("executedlcrefund")
        .hidden()
        .action((_, conf) => conf.copy(command = ExecuteDLCRefund(null)))
        .text("Executes the Refund transaction for the given DLC")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case executeDLCRefund: ExecuteDLCRefund =>
                  executeDLCRefund.copy(eventId = eventId)
                case other => other
              }))
        ),
      cmd("claimdlcpenaltyfunds")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = ClaimDLCPenaltyFunds(null, EmptyTransaction)))
        .text("Claims the penalty funds for the corresponding DLC")
        .children(
          opt[Sha256DigestBE]("eventid").required
            .action((eventId, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCPenaltyFunds: ClaimDLCPenaltyFunds =>
                  claimDLCPenaltyFunds.copy(eventId = eventId)
                case other => other
              })),
          opt[Transaction]("forceclosetx")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case claimDLCPenaltyFunds: ClaimDLCPenaltyFunds =>
                  claimDLCPenaltyFunds.copy(forceCloseTx = tx)
                case other => other
              }))
        ),
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
      cmd("combinepsbts")
        .hidden()
        .action((_, conf) => conf.copy(command = CombinePSBTs(Seq.empty)))
        .text("Combines all the given PSBTs")
        .children(
          opt[Seq[PSBT]]("psbts")
            .required()
            .action((seq, conf) =>
              conf.copy(command = conf.command match {
                case combinePSBTs: CombinePSBTs =>
                  combinePSBTs.copy(psbts = seq)
                case other => other
              }))
        ),
      cmd("joinpsbts")
        .hidden()
        .action((_, conf) => conf.copy(command = JoinPSBTs(Seq.empty)))
        .text("Combines all the given PSBTs")
        .children(
          opt[Seq[PSBT]]("psbts")
            .required()
            .action((seq, conf) =>
              conf.copy(command = conf.command match {
                case joinPSBTs: JoinPSBTs =>
                  joinPSBTs.copy(psbts = seq)
                case other => other
              }))
        ),
      cmd("finalizepsbt")
        .hidden()
        .action((_, conf) => conf.copy(command = FinalizePSBT(PSBT.empty)))
        .text("Finalizes the given PSBT if it can")
        .children(
          opt[PSBT]("psbt")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case finalizePSBT: FinalizePSBT =>
                  finalizePSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("extractfrompsbt")
        .hidden()
        .action((_, conf) => conf.copy(command = ExtractFromPSBT(PSBT.empty)))
        .text("Extracts a transaction from the given PSBT if it can")
        .children(
          opt[PSBT]("psbt")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case extractFromPSBT: ExtractFromPSBT =>
                  extractFromPSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("converttopsbt")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = ConvertToPSBT(EmptyTransaction)))
        .text("Creates an empty psbt from the given transaction")
        .children(
          opt[Transaction]("unsignedTx")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case convertToPSBT: ConvertToPSBT =>
                  convertToPSBT.copy(transaction = tx)
                case other => other
              }))
        ),
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

  def exec(args: String*): Try[String] = {
    val config = OParser.parse(parser, args.toVector, Config()) match {
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
    def error[T](message: String): Failure[T] = {
      Failure(new RuntimeException(message))
    }

    val requestParam: RequestParam = config.command match {
      // DLCs
      case CreateDLCOffer(oracleInfo,
                          contractInfo,
                          collateral,
                          feeRateOpt,
                          locktime,
                          refundLT,
                          escaped) =>
        RequestParam(
          "createdlcoffer",
          Seq(
            up.writeJs(oracleInfo),
            up.writeJs(contractInfo),
            up.writeJs(collateral),
            up.writeJs(feeRateOpt),
            up.writeJs(locktime),
            up.writeJs(refundLT),
            up.writeJs(escaped)
          )
        )
      case AcceptDLCOffer(offer, escaped) =>
        RequestParam("acceptdlcoffer",
                     Seq(up.writeJs(offer), up.writeJs(escaped)))
      case SignDLC(accept, escaped) =>
        RequestParam("signdlc", Seq(up.writeJs(accept), up.writeJs(escaped)))
      case AddDLCSigs(sigs) =>
        RequestParam("adddlcsigs", Seq(up.writeJs(sigs)))
      case InitDLCMutualClose(eventId, oracleSig, escaped) =>
        RequestParam(
          "initdlcmutualclose",
          Seq(up.writeJs(eventId), up.writeJs(oracleSig), up.writeJs(escaped)))
      case AcceptDLCMutualClose(mutualCloseSig) =>
        RequestParam("acceptdlcmutualclose", Seq(up.writeJs(mutualCloseSig)))
      case ExecuteDLCUnilateralClose(eventId, oracleSig) =>
        RequestParam("executedlcunilateralclose",
                     Seq(up.writeJs(eventId), up.writeJs(oracleSig)))
      case GetDLCFundingTx(eventId) =>
        RequestParam("getdlcfundingtx", Seq(up.writeJs(eventId)))
      case ExecuteDLCForceClose(eventId, oracleSig) =>
        RequestParam("executedlcforceclose",
                     Seq(up.writeJs(eventId), up.writeJs(oracleSig)))
      case ClaimDLCRemoteFunds(eventId, forceCloseTx) =>
        RequestParam("claimdlcremotefunds",
                     Seq(up.writeJs(eventId), up.writeJs(forceCloseTx)))
      case ExecuteDLCRefund(eventId) =>
        RequestParam("executedlcrefund", Seq(up.writeJs(eventId)))
      case ClaimDLCPenaltyFunds(eventId, forceCloseTx) =>
        RequestParam("claimdlcpenaltyfunds",
                     Seq(up.writeJs(eventId), up.writeJs(forceCloseTx)))
      // Wallet
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
      case GetPeers => RequestParam("getpeers")
      // PSBTs
      case CombinePSBTs(psbts) =>
        RequestParam("combinepsbts", Seq(up.writeJs(psbts)))
      case JoinPSBTs(psbts) =>
        RequestParam("joinpsbts", Seq(up.writeJs(psbts)))
      case FinalizePSBT(psbt) =>
        RequestParam("finalizepsbt", Seq(up.writeJs(psbt)))
      case ExtractFromPSBT(psbt) =>
        RequestParam("extractfrompsbt", Seq(up.writeJs(psbt)))
      case ConvertToPSBT(tx) =>
        RequestParam("converttopsbt", Seq(up.writeJs(tx)))

      case NoCommand => ???
    }

    Try {
      import com.softwaremill.sttp._
      implicit val backend = HttpURLConnectionBackend()
      val request =
        sttp
          .post(uri"http://$host:$port/")
          .contentType("application/json")
          .body({
            val uuid = java.util.UUID.randomUUID.toString
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
      val jsObjT =
        Try(js.obj).transform[mutable.LinkedHashMap[String, ujson.Value]](
          Success(_),
          _ => error(s"Response was not a JSON object! Got: $rawBody"))

      /** Gets the given key from jsObj if it exists
        * and is not null */
      def getKey(key: String): Option[ujson.Value] = {
        jsObjT.toOption.flatMap(_.get(key).flatMap(result =>
          if (result.isNull) None else Some(result)))
      }

      /** Converts a `ujson.Value` to String, making an
        * effort to avoid preceding and trailing `"`s */
      def jsValueToString(value: ujson.Value) = value match {
        case Str(string)             => string
        case Num(num) if num.isWhole => num.toLong.toString
        case Num(num)                => num.toString
        case rest: ujson.Value       => rest.toString()
      }

      (getKey("result"), getKey("error")) match {
        case (Some(result), None) =>
          Success(jsValueToString(result))
        case (None, Some(err)) =>
          val msg = jsValueToString(err)
          error(msg)
        case (None, None) | (Some(_), Some(_)) =>
          error(s"Got unexpected response: $rawBody")
      }
    }.flatten
  }

  // TODO make this dynamic
  def port = 9999
  def host = "localhost"

  case class RequestParam(
      method: String,
      params: Seq[ujson.Value.Value] = Nil) {

    lazy val toJsonMap: Map[String, ujson.Value] = {
      Map("method" -> method, "params" -> params)
    }
  }
}

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
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      collateral: Satoshis,
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      escaped: Boolean)
      extends CliCommand

  case class AcceptDLCOffer(offer: DLCOffer, escaped: Boolean)
      extends CliCommand

  case class SignDLC(accept: DLCAccept, escaped: Boolean) extends CliCommand

  case class AddDLCSigs(sigs: DLCSign) extends CliCommand

  case class InitDLCMutualClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature,
      escaped: Boolean)
      extends CliCommand

  case class AcceptDLCMutualClose(mutualCloseSig: DLCMutualCloseSig)
      extends CliCommand

  case class GetDLCFundingTx(eventId: Sha256DigestBE) extends CliCommand

  case class ExecuteDLCUnilateralClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature)
      extends CliCommand

  case class ExecuteDLCForceClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature)
      extends CliCommand

  case class ClaimDLCRemoteFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction)
      extends CliCommand

  case class ExecuteDLCRefund(eventId: Sha256DigestBE) extends CliCommand

  case class ClaimDLCPenaltyFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction)
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

  // PSBT
  case class CombinePSBTs(psbts: Seq[PSBT]) extends CliCommand
  case class JoinPSBTs(psbts: Seq[PSBT]) extends CliCommand
  case class FinalizePSBT(psbt: PSBT) extends CliCommand
  case class ExtractFromPSBT(psbt: PSBT) extends CliCommand
  case class ConvertToPSBT(transaction: Transaction) extends CliCommand
}
