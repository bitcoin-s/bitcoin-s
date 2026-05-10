package org.bitcoins.cli.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.bitcoins.server.grpc.{
  CommonRoutesClient,
  GetVersionRequest,
  GetVersionResponse,
  ZipDataDirRequest
}
import ujson.{Null, Num, Str}

import java.io.File
import java.nio.file.Path
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object ConsoleCliGrpc {

  private val usage =
    """bitcoin-s-cli-grpc [--host <host>] [--rpcport <port>] getversion
      |bitcoin-s-cli-grpc [--host <host>] [--rpcport <port>] zipdatadir <path>""".stripMargin

  def exec(args: Vector[String])(implicit
      system: ActorSystem): Future[String] = {
    parseArgs(args) match {
      case Success(conf) => exec(conf.command, conf)
      case Failure(err)  => Future.failed(err)
    }
  }

  private def parseArgs(args: Vector[String]): Try[Config] = {
    def loop(remaining: Vector[String], conf: Config): Try[Config] = {
      remaining match {
        case Vector() =>
          conf.command match {
            case NoCommand =>
              Failure(
                new IllegalArgumentException("You need to provide a command!"))
            case _ => Success(conf)
          }
        case Vector("-h") | Vector("--help") =>
          Success(conf.copy(command = Help))
        case "--host" +: host +: tail =>
          loop(tail, conf.copy(host = host))
        case "--rpcport" +: port +: tail =>
          Try(port.toInt).transform(
            p => loop(tail, conf.copy(rpcPortOpt = Some(p))),
            _ =>
              Failure(new IllegalArgumentException(s"Invalid rpcport: $port"))
          )
        case "getversion" +: Vector() =>
          Success(conf.copy(command = GetVersion))
        case "zipdatadir" +: path +: Vector() =>
          Success(conf.copy(command = ZipDataDir(new File(path).toPath)))
        case "zipdatadir" +: Vector() =>
          Failure(new IllegalArgumentException("Missing path argument"))
        case unknown +: _ =>
          Failure(new IllegalArgumentException(s"Unknown argument '$unknown'"))
        case _ =>
          Failure(new IllegalArgumentException("Failed to parse arguments"))
      }
    }

    loop(args, Config())
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
      case Help =>
        Future.successful(usage)
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

case object Help extends CliGrpcCommand

case object GetVersion extends CliGrpcCommand

case class ZipDataDir(path: Path) extends CliGrpcCommand

case class Config(
    command: CliGrpcCommand = NoCommand,
    host: String = "localhost",
    rpcPortOpt: Option[Int] = None
) {
  val rpcPort: Int = rpcPortOpt.getOrElse(command.defaultPort)
}
