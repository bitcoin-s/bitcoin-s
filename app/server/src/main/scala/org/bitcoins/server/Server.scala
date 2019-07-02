package org.bitcoins.server

import upickle.{default => up}
import akka.actor.ActorSystem
import akka.http.scaladsl._
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._

import de.heikoseeberger.akkahttpupickle.UpickleSupport._

case class Server(handlers: Seq[ServerRoute])(implicit system: ActorSystem)
    extends BitcoinSLogger {
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  private val exceptionHandler = ExceptionHandler {
    case HttpError.MethodNotFound(method) =>
      complete(
        HttpResponse(StatusCodes.NotFound,
                     entity =
                       Server.httpError(s"'$method' is not a valid method")))
  }

  /** Handles all server commands by throwing a MethodNotFound */
  private val catchAllHandler: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand(name, _) => throw HttpError.MethodNotFound(name)
  }

  val route = handleExceptions(exceptionHandler) {
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

  def start() = {
    val httpFut = Http().bindAndHandle(route, "localhost", 9999)
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
          case Some(res) => res
          case None      => ujson.Null
        }),
        "error" -> (error match {
          case Some(err) => err
          case None      => ujson.Null
        })
      )
    }
  }

  /** Creates a HTTP response with the given body as a JSON response */
  def httpSuccess[T](body: T)(
      implicit writer: up.Writer[T]): HttpEntity.Strict = {
    val response = Response(result = Some(up.write(body)))
    HttpEntity(
      ContentTypes.`application/json`,
      up.write(response.toJsonMap)
    )
  }

  def httpError(msg: String): HttpEntity.Strict = {
    val response = Response(error = Some(msg))
    HttpEntity(
      ContentTypes.`application/json`,
      up.write(response.toJsonMap)
    )
  }
}
