package org.bitcoins.server.util

import akka.http.scaladsl.Http

import scala.concurrent.{ExecutionContext, Future}

case class ServerBindings(
    httpServer: Http.ServerBinding,
    webSocketServerOpt: Option[Http.ServerBinding]) {

  def stop()(implicit ec: ExecutionContext): Future[Unit] = {
    val stopHttp = httpServer.unbind()
    val stopWsFOpt = webSocketServerOpt.map(_.unbind())
    for {
      _ <- stopHttp
      _ <- stopWsFOpt match {
        case Some(doneF) =>
          doneF
        case None =>
          Future.unit
      }
    } yield ()
  }
}
