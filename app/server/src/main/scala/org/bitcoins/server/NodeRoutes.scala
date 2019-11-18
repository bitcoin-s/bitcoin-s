package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import org.bitcoins.node.Node

import scala.util.{Failure, Success}

case class NodeRoutes(node: Node)(implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getpeers", _) =>
      complete {
        Server.httpSuccess("TODO implement getpeers")
      }
    case ServerCommand("rescan", arr) =>
      Rescan.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(Rescan(addresses, startBlock, endBlock)) =>
          complete {
            node
              .rescan(addresses.map(_.scriptPubKey), startBlock, endBlock)
              .map(_ => Server.httpSuccess("ok"))
          }
      }
  }
}
