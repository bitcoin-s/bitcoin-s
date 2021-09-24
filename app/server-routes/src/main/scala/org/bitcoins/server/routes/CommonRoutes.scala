package org.bitcoins.server.routes

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.RouteDirectives
import org.bitcoins.core.util.EnvUtil

case class CommonRoutes() extends ServerRoute {

  override def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("getversion", _) =>
      RouteDirectives.complete {
        val vec = Vector(("version", ujson.Str(EnvUtil.getVersion)))
        val obj = ujson.Obj.from(vec)
        Server.httpSuccess(obj)
      }
  }
}
