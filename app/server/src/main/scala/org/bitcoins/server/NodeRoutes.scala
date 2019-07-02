package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.SpvNode

case class NodeRoutes(node: SpvNode)(implicit system: ActorSystem)
    extends BitcoinSLogger
    with ServerRoute {
  implicit val materializer = ActorMaterializer()

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getpeers", _) =>
      complete {
        Server.httpSuccess("TODO implement getpeers")
      }
  }
}
