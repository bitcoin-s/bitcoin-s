package org.bitcoins.server
import akka.http.scaladsl.server.StandardRoute

trait ServerRoute {
  def handleCommand: PartialFunction[ServerCommand, StandardRoute]
}
