package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.server.routes._

case class DLCRoutes(dlcNode: DLCNodeApi)(implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("getdlchostaddress", _) =>
      complete {
        dlcNode.getHostAddress.map { addr =>
          // need to do this otherwise string will contain <unresolved>
          val str = s"${addr.getHostName}:${addr.getPort}"
          Server.httpSuccess(str)
        }
      }

    case ServerCommand("acceptdlc", arr) =>
      withValidServerCommand(AcceptDLC.fromJsArr(arr)) {
        case AcceptDLC(offer, address) =>
          complete {
            dlcNode.acceptDLCOffer(address, offer).map { _ =>
              Server.httpSuccess(ujson.Null)
            }
          }
      }
  }
}
