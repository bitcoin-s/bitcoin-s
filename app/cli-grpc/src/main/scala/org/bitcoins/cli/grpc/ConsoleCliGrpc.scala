package org.bitcoins.cli.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.bitcoins.cli.CliCommand.{
  GetBestBlockHash,
  GetBlockCount,
  GetFilterCount,
  GetFilterHeaderCount,
  GetInfo,
  GetMedianTimePast,
  GetVersion,
  ZipDataDir
}
import org.bitcoins.cli.{Config, ConsoleCli}
import org.bitcoins.commons.rpc.{
  AppServerCliCommand,
  CliCommand,
  CliGrpcCommand,
  GetBlockHeader,
  OracleServerCliCommand,
  ServerlessCliCommand
}
import org.bitcoins.commons.rpc.CliCommand.NoCommand
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

import scala.concurrent.Future
import scala.util.{Failure, Success}

object ConsoleCliGrpc {

  def parser: OParser[Unit, Config] = ConsoleCli.parser

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
    val normalized = normalizeArgs(args)
    OParser.parse(parser, normalized, Config()) match {
      case None =>
        Future.failed(
          new RuntimeException(
            s"Invalid arguments provided. See usage above. args=$normalized"))
      case Some(conf) =>
        conf.command match {
          case c: CliGrpcCommand =>
            exec(c, conf)
          case _: AppServerCliCommand | _: ServerlessCliCommand |
              _: OracleServerCliCommand =>
            Future.failed(
              new RuntimeException(
                s"Command ${conf.command} is not supported in gRPC mode"))
        }

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
      .connectToServiceAt(ConsoleCli.host, config.rpcPort)
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
      case x: CliCommand =>
        Future.failed(
          new IllegalArgumentException(
            s"Command $x is not supported in gRPC mode"))
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
