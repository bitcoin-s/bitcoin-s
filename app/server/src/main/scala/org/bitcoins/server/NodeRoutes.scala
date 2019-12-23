package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import org.bitcoins.node.Node

case class NodeRoutes(node: Node)(implicit system: ActorSystem)
    extends ServerRoute {
  implicit val materializer = ActorMaterializer()

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getpeers", _) =>
      complete {
        Server.httpSuccess("TODO implement getpeers")
      }
  }
}
