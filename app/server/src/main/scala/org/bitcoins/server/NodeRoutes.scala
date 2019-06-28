package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.SpvNode

case class NodeRoutes(node: SpvNode)(implicit system: ActorSystem)
    extends BitcoinSLogger {
  implicit val materializer = ActorMaterializer()

  val routes: Route = {
    pathPrefix("node") {
      path("getpeers") {
        complete {
          HttpEntity(
            ContentTypes.`application/json`,
            "idk there's probably some peers here"
          )
        }
      }
    }
  }
}
