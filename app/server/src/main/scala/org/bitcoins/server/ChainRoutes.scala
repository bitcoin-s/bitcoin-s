package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.chain.api.ChainApi

case class ChainRoutes(chain: ChainApi)(implicit system: ActorSystem)
    extends BitcoinSLogger {
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val routes: Route = {
    pathPrefix("chain") {
      path("getblockcount") {
        complete {
          chain.getBlockCount.map { count =>
            HttpEntity(
              ContentTypes.`application/json`,
              count.toString
            )
          }
        }
      } ~ path("getbestblockhash") {
        complete {
          chain.getBestBlockHash.map { hash =>
            HttpEntity(ContentTypes.`application/json`, hash.hex)
          }
        }
      }
    }
  }
}
