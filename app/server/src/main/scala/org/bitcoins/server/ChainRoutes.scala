package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import org.bitcoins.chain.api.ChainApi

import org.bitcoins.picklers._

case class ChainRoutes(chain: ChainApi)(implicit system: ActorSystem)
    extends ServerRoute {
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getblockcount", _) =>
      complete {
        chain.getBlockCount.map { count =>
          Server.httpSuccess(count)
        }
      }
    case ServerCommand("getbestblockhash", _) =>
      complete {
        chain.getBestBlockHash.map { hash =>
          Server.httpSuccess(hash)
        }
      }
  }

}
