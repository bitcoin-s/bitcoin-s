package org.bitcoins.server

import upickle.{default => up}
import akka.actor.ActorSystem
import akka.http.scaladsl._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._

import de.heikoseeberger.akkahttpupickle.UpickleSupport._
import akka.http.scaladsl.server.directives.DebuggingDirectives
import akka.event.Logging
import org.bitcoins.db.HttpLogger
import org.bitcoins.db.AppConfig

case class Server(conf: AppConfig, handlers: Seq[ServerRoute])(
    implicit system: ActorSystem)
    extends HttpLogger {
  implicit private val config: AppConfig = conf

  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  /** Handles all server commands by throwing a MethodNotFound */
  private val catchAllHandler: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand(name, _) => throw HttpError.MethodNotFound(name)
  }

  /** HTTP directive that handles both exceptions and rejections */
  private def withErrorHandling(route: Route): Route = {

    val rejectionHandler =
      RejectionHandler
        .newBuilder()
        .handleNotFound {
          complete {
            Server.httpError(
              """Resource not found. Hint: all RPC calls are made against root ('/')""",
              StatusCodes.BadRequest)
          }
        }
        .result()

    val exceptionHandler = ExceptionHandler {
      case HttpError.MethodNotFound(method) =>
        complete(
          Server.httpError(s"'$method' is not a valid method",
                           StatusCodes.BadRequest))
      case err: Throwable =>
        logger.info(s"Unhandled error in server:", err)
        complete(Server.httpError("There was an error"))
    }

    handleRejections(rejectionHandler) {
      handleExceptions(exceptionHandler) {
        route
      }
    }
  }

  val route =
    // TODO implement better logging
    DebuggingDirectives.logRequestResult("http-rpc-server", Logging.InfoLevel) {
      withErrorHandling {
        pathSingleSlash {
          post {
            entity(as[ServerCommand]) { cmd =>
              val init = PartialFunction.empty[ServerCommand, StandardRoute]
              val handler = handlers.foldLeft(init) {
                case (accum, curr) => accum.orElse(curr.handleCommand)
              }
              handler.orElse(catchAllHandler).apply(cmd)
            }
          }
        }
      }
    }

  def start() = {
    val httpFut =
      Http().bindAndHandle(route, "localhost", 9999)
    httpFut.foreach { http =>
      logger.info(s"Started Bitcoin-S HTTP server at ${http.localAddress}")
    }
    httpFut
  }
}

object Server {

  // TODO id parameter
  case class Response(
      result: Option[ujson.Value] = None,
      error: Option[String] = None) {

    def toJsonMap: Map[String, ujson.Value] = {
      Map(
        "result" -> (result match {
          case None      => ujson.Null
          case Some(res) => res
        }),
        "error" -> (error match {
          case None      => ujson.Null
          case Some(err) => err
        })
      )
    }
  }

  /** Creates a HTTP response with the given body as a JSON response */
  def httpSuccess[T](body: T)(
      implicit writer: up.Writer[T]): HttpEntity.Strict = {
    val response = Response(result = Some(up.writeJs(body)))
    HttpEntity(
      ContentTypes.`application/json`,
      up.write(response.toJsonMap)
    )
  }

  def httpError(
      msg: String,
      status: StatusCode = StatusCodes.InternalServerError): HttpResponse = {

    val entity = {
      val response = Response(error = Some(msg))
      HttpEntity(
        ContentTypes.`application/json`,
        up.write(response.toJsonMap)
      )
    }

    HttpResponse(status = status, entity = entity)
  }
}
