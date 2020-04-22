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
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getpeers", _) =>
      complete {
        Server.httpSuccess("TODO implement getpeers")
      }

    case ServerCommand("stop", _) =>
      complete {
        val nodeStopping = node.stop().map { _ =>
          Server.httpSuccess("Node shutting down")
        }
        system.terminate()
        nodeStopping
      }

    case ServerCommand("sendrawtransaction", arr) =>
      SendRawTransaction.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SendRawTransaction(tx)) =>
          complete {
            node.broadcastTransaction(tx).map { _ =>
              Server.httpSuccess(s"${tx.txIdBE.hex}")
            }
          }
      }
  }
}
