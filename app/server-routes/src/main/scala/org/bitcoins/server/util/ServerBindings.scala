package org.bitcoins.server.util

import akka.http.scaladsl.Http

import scala.concurrent.{ExecutionContext, Future}

case class ServerBindings(
    httpServer: Http.ServerBinding,
    webSocketServer: Http.ServerBinding) {

  def stop()(implicit ec: ExecutionContext): Future[Unit] = {
    val stopHttp = httpServer.unbind()
    val stopWs = webSocketServer.unbind()
    for {
      _ <- stopHttp
      _ <- stopWs
    } yield ()
  }
}
