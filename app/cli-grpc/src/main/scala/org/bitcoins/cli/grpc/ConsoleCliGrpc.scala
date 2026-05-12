package org.bitcoins.cli.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.bitcoins.commons.config.AppConfig
import org.bitcoins.server.grpc.{
  CommonRoutesClient,
  GetVersionRequest,
  GetVersionResponse,
  ZipDataDirRequest
}
import scopt.OParser
import ujson.{Null, Num, Str}

import java.io.File
import java.nio.file.Path
import scala.concurrent.Future
import scala.util.{Failure, Success}

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
      checkConfig {
        case Config(NoCommand, _, _) =>
          failure("You need to provide a command!")
        case _ => success
      }
    )
  }

  // Global options that consume the following argument
  private val globalOptsWithArg: Set[String] = Set("--host", "--rpcport")

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

    val clientSettings = GrpcClientSettings
      .connectToServiceAt(config.host, config.rpcPort)
      .withTls(false)

    val client = CommonRoutesClient(clientSettings)

    val responseF = command match {
      case GetVersion =>
        client.getVersion(GetVersionRequest()).map(formatGetVersion)
      case ZipDataDir(path) =>
        client.zipDataDir(ZipDataDirRequest(path = path.toString)).map(_ => "")
      case NoCommand =>
        Future.failed(
          new IllegalArgumentException("You need to provide a command!"))
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

sealed trait CliGrpcCommand {
  def defaultPort: Int = 8980
}

case object NoCommand extends CliGrpcCommand

case object GetVersion extends CliGrpcCommand

case class ZipDataDir(path: Path) extends CliGrpcCommand

case class Config(
    command: CliGrpcCommand = NoCommand,
    host: String = "localhost",
    rpcPortOpt: Option[Int] = None
) {
  val rpcPort: Int = rpcPortOpt.getOrElse(command.defaultPort)
}
