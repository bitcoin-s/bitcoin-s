package org.bitcoins.server

import akka.http.scaladsl.server.Route
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import org.bitcoins.core.util.BitcoinSLogger

case class Server(routes: Route)(implicit system: ActorSystem)
    extends BitcoinSLogger {
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  def start() = {
    val httpFut = Http().bindAndHandle(routes, "localhost", 9999)
    httpFut.foreach { http =>
      logger.info(s"Started Bitcoin-S HTTP server at ${http.localAddress}")
    }
    httpFut
  }
}
