package org.bitcoins.cli.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.server.grpc.{
  ChainRoutesClient,
  GetBestBlockHashRequest,
  GetBlockCountRequest,
  GetBlockHeaderRequest,
  GetBlockHeaderResponse,
  GetFilterCountRequest,
  GetFilterHeaderCountRequest,
  GetInfoRequest,
  GetInfoResponse,
  GetMedianTimePastRequest,
  CommonRoutesClient,
  GetVersionRequest,
  GetVersionResponse,
  GrpcAuth,
  ZipDataDirRequest
}
import scopt.OParser
import ujson.{Null, Num, Str}

import java.io.File
import java.nio.file.Path
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object ConsoleCliGrpc {

  def parser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("bitcoin-s-cli-grpc"),
      opt[String]("host")
        .action((host, conf) => conf.copy(host = host))
        .text("The hostname of the bitcoin-s gRPC server"),
      opt[Int]("rpcport")
        .action((port, conf) => conf.copy(rpcPortOpt = Some(port)))
        .text("The port of the bitcoin-s gRPC server"),
      opt[String]("password")
        .action((password, conf) => conf.copy(rpcPassword = password))
        .text("The password to authenticate to the bitcoin-s gRPC server"),
      help('h', "help").text("Display this help message and exit"),
      cmd("getversion")
        .action((_, conf) => conf.copy(command = GetVersion))
        .text("Returns the version of the bitcoin-s server"),
      cmd("zipdatadir")
        .action((_, conf) =>
          conf.copy(command = ZipDataDir(AppConfig.DEFAULT_BITCOIN_S_DATADIR)))
        .text("Zips the bitcoin-s data directory to the given path")
        .children(
          arg[File]("path")
            .text("The destination path for the zipped data directory")
            .required()
            .action((file, conf) =>
              conf.copy(command = conf.command match {
                case _: ZipDataDir => ZipDataDir(file.toPath)
                case other         => other
              }))
        ),
      cmd("getinfo")
        .action((_, conf) => conf.copy(command = GetInfo))
        .text("Returns basic info about the current chain"),
      cmd("getblockcount")
        .action((_, conf) => conf.copy(command = GetBlockCount))
        .text("Get the block height"),
      cmd("getfiltercount")
        .action((_, conf) => conf.copy(command = GetFilterCount))
        .text("Get the number of filters"),
      cmd("getfilterheadercount")
        .action((_, conf) => conf.copy(command = GetFilterHeaderCount))
        .text("Get the number of filter headers"),
      cmd("getbestblockhash")
        .action((_, conf) => conf.copy(command = GetBestBlockHash))
        .text("Get the best block hash"),
      cmd("getblockheader")
        .action((_, conf) =>
          conf.copy(command = GetBlockHeader(DoubleSha256DigestBE.empty)))
        .text("Returns information about block header <hash>")
        .children(
          arg[String]("hash")
            .text("The block hash")
            .required()
            .validate(hash =>
              Try(DoubleSha256DigestBE(hash))
                .map(_ => success)
                .getOrElse(failure("Invalid block hash")))
            .action((hash, conf) =>
              conf.copy(command = conf.command match {
                case gbh: GetBlockHeader =>
                  gbh.copy(hash = DoubleSha256DigestBE(hash))
                case other => other
              }))
        ),
      cmd("getmediantimepast")
        .action((_, conf) => conf.copy(command = GetMedianTimePast))
        .text("Get the median time past"),
      checkConfig {
        case Config(NoCommand, _, _, _) =>
          failure("You need to provide a command!")
        case _ => success
      }
    )
  }

  // Global options that consume the following argument
  private val globalOptsWithArg: Set[String] =
    Set("--host", "--rpcport", "--password")

  // scopt's cmd() requires the subcommand token to precede any global options.
  // This reorders args so leading global options are moved after the first
  // non-option token (the command name), preserving the original semantics.
  private def normalizeArgs(args: Vector[String]): Vector[String] = {
    @annotation.tailrec
    def peel(
        remaining: Vector[String],
        globalOpts: Vector[String]
    ): (Vector[String], Vector[String]) =
      remaining match {
        case opt +: value +: tail if globalOptsWithArg.contains(opt) =>
          peel(tail, globalOpts :+ opt :+ value)
        case flag +: tail if flag.startsWith("-") =>
          peel(tail, globalOpts :+ flag)
        case _ => (remaining, globalOpts)
      }

    val (cmdAndArgs, globalOpts) = peel(args, Vector.empty)
    cmdAndArgs ++ globalOpts
  }

  def exec(args: Vector[String])(implicit
      system: ActorSystem): Future[String] = {
    OParser.parse(parser, normalizeArgs(args), Config()) match {
      case None =>
        Future.failed(
          new RuntimeException("Invalid arguments provided. See usage above."))
      case Some(conf) => exec(conf.command, conf)
    }
  }

  private def jsValueToString(value: ujson.Value): String = {
    value match {
      case Str(string)             => string
      case Num(num) if num.isWhole => num.toLong.toString
      case Num(num)                => num.toString
      case rest: ujson.Value       => rest.render(2)
    }
  }

  def exec(command: CliGrpcCommand, config: Config)(implicit
      system: ActorSystem
  ): Future[String] = {
    import system.dispatcher

    val baseSettings = GrpcClientSettings
      .connectToServiceAt(config.host, config.rpcPort)
      .withTls(false)
    val clientSettings =
      if (config.rpcPassword.isEmpty) {
        baseSettings
      } else {
        baseSettings.withCallCredentials(
          GrpcAuth
            .basicCallCredentials(config.rpcPassword))
      }

    val commonClient = CommonRoutesClient(clientSettings)
    val chainClient = ChainRoutesClient(clientSettings)

    val responseF = command match {
      case GetVersion =>
        commonClient.getVersion(GetVersionRequest()).map(formatGetVersion)
      case ZipDataDir(path) =>
        commonClient
          .zipDataDir(ZipDataDirRequest(path = path.toString))
          .map(_ => "")
      case GetInfo =>
        chainClient.getInfo(GetInfoRequest()).map(formatGetInfo)
      case GetBlockCount =>
        chainClient
          .getBlockCount(GetBlockCountRequest())
          .map(r => jsValueToString(Num(r.count)))
      case GetFilterCount =>
        chainClient
          .getFilterCount(GetFilterCountRequest())
          .map(r => jsValueToString(Num(r.count)))
      case GetFilterHeaderCount =>
        chainClient
          .getFilterHeaderCount(GetFilterHeaderCountRequest())
          .map(r => jsValueToString(Num(r.count)))
      case GetBestBlockHash =>
        chainClient
          .getBestBlockHash(GetBestBlockHashRequest())
          .map(r => jsValueToString(Str(r.hash)))
      case GetBlockHeader(hash) =>
        chainClient
          .getBlockHeader(GetBlockHeaderRequest(hash = hash.hex))
          .map(formatGetBlockHeader)
      case GetMedianTimePast =>
        chainClient
          .getMedianTimePast(GetMedianTimePastRequest())
          .map(r => r.mediantimepast.toString)
      case NoCommand =>
        Future.failed(
          new IllegalArgumentException("You need to provide a command!"))
    }

    responseF.transformWith {
      case Success(result) =>
        for {
          _ <- commonClient.close()
          _ <- chainClient.close()
        } yield result
      case Failure(err) =>
        for {
          _ <- commonClient.close()
          _ <- chainClient.close()
          result <- Future.failed(err)
        } yield result
    }
  }

  private def formatGetVersion(response: GetVersionResponse): String = {
    val version =
      response.version.map(Str.apply).getOrElse(Null)

    jsValueToString(ujson.Obj("version" -> version))
  }

  private def formatGetInfo(response: GetInfoResponse): String = {
    jsValueToString(
      ujson.Obj(
        "network" -> response.network,
        "blockHeight" -> response.blockHeight,
        "blockHash" -> response.blockHash,
        "torStarted" -> response.torStarted,
        "syncing" -> response.syncing,
        "isinitialblockdownload" -> response.isInitialBlockDownload
      ))
  }

  private def formatGetBlockHeader(response: GetBlockHeaderResponse): String = {
    response.header match {
      case Some(header) =>
        jsValueToString(
          ujson.Obj(
            "hash" -> header.hash,
            "confirmations" -> header.confirmations,
            "height" -> header.height,
            "version" -> header.version,
            "versionHex" -> header.versionHex,
            "merkleroot" -> header.merkleroot,
            "time" -> header.time,
            "mediantime" -> header.mediantime,
            "nonce" -> header.nonce,
            "bits" -> header.bits,
            "difficulty" -> header.difficulty,
            "chainwork" -> header.chainwork,
            "previousblockhash" -> header.previousblockhash
              .map(ujson.Str.apply)
              .getOrElse(ujson.Null),
            "nextblockhash" -> header.nextblockhash
              .map(ujson.Str.apply)
              .getOrElse(ujson.Null),
            "target" -> header.target.map(ujson.Str.apply).getOrElse(ujson.Null)
          ))
      case None => "null"
    }
  }
}

sealed trait CliGrpcCommand {
  def defaultPort: Int = 8980
}

case object NoCommand extends CliGrpcCommand

case object GetVersion extends CliGrpcCommand

case class ZipDataDir(path: Path) extends CliGrpcCommand

case object GetInfo extends CliGrpcCommand

case object GetBlockCount extends CliGrpcCommand

case object GetFilterCount extends CliGrpcCommand

case object GetFilterHeaderCount extends CliGrpcCommand

case object GetBestBlockHash extends CliGrpcCommand

case class GetBlockHeader(hash: DoubleSha256DigestBE) extends CliGrpcCommand

case object GetMedianTimePast extends CliGrpcCommand

case class Config(
    command: CliGrpcCommand = NoCommand,
    host: String = "localhost",
    rpcPortOpt: Option[Int] = None,
    rpcPassword: String = ""
) {
  val rpcPort: Int = rpcPortOpt.getOrElse(command.defaultPort)
}
