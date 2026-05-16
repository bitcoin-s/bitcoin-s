package org.bitcoins.cli.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.bitcoins.cli.CliCommand.{GetVersion, ZipDataDir}
import org.bitcoins.cli.{Config, ConsoleCli}
import org.bitcoins.commons.rpc.CliCommand
import org.bitcoins.commons.rpc.CliCommand.NoCommand
import org.bitcoins.server.grpc.{
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

  def exec(command: CliCommand, config: Config)(implicit
      system: ActorSystem
  ): Future[String] = {
    import system.dispatcher

    val baseSettings = GrpcClientSettings
      .connectToServiceAt("localhost", config.rpcPort)
      .withTls(false)
    val clientSettings =
      if (config.rpcPassword.isEmpty) {
        baseSettings
      } else {
        baseSettings.withCallCredentials(
          GrpcAuth
            .basicCallCredentials(config.rpcPassword))
      }

    val client = CommonRoutesClient(clientSettings)

    val responseF = command match {
      case GetVersion =>
        client.getVersion(GetVersionRequest()).map(formatGetVersion)
      case ZipDataDir(path) =>
        client.zipDataDir(ZipDataDirRequest(path = path.toString)).map(_ => "")
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
        client.close().map(_ => result)
      case Failure(err) =>
        client.close().flatMap(_ => Future.failed(err))
    }
  }

  private def formatGetVersion(response: GetVersionResponse): String = {
    val version =
      response.version.map(Str.apply).getOrElse(Null)

    jsValueToString(ujson.Obj("version" -> version))
  }
}
