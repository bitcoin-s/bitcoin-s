package org.bitcoins.server

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.server.Directives.complete
import org.apache.pekko.http.scaladsl.server.Route
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.node.Node
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.routes.{Server, ServerCommand, ServerRoute}

import scala.concurrent.duration.DurationInt

case class NodeRoutes(nodeApi: NodeApi)(implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher

  override def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("getconnectioncount", _) =>
      complete {
        nodeApi.getConnectionCount
          .map(count => Server.httpSuccess(count))
      }
    case ServerCommand("stop", _) =>
      nodeApi match {
        case node: Node =>
          complete {
            val nodeStopping = node.stop().map { _ =>
              Server.httpSuccess("Node shutting down")
            }

            system.scheduler.scheduleOnce(7.seconds)(sys.exit())
            nodeStopping
          }
        case bitcoind: BitcoindRpcClient =>
          complete {
            val nodeStopping = bitcoind.stop().map { _ =>
              Server.httpSuccess("Node shutting down")
            }

            system.scheduler.scheduleOnce(7.seconds)(sys.exit())
            nodeStopping
          }
        case n: NodeApi =>
          sys.error(s"Unsupported NodeApi type=$n")
      }
  }
}
