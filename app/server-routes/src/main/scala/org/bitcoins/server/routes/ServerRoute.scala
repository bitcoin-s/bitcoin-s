package org.bitcoins.server.routes

import org.apache.pekko.http.scaladsl.server.Directives.{complete, provide}
import org.apache.pekko.http.scaladsl.server.{Directive1, Route}

import scala.util.Try

trait ServerRoute {
  def handleCommand: PartialFunction[ServerCommand, Route]

  def withValidServerCommand[R](validator: Try[R]): Directive1[R] =
    validator.fold(
      e => complete(Server.httpBadRequest(e)),
      provide
    )
}
