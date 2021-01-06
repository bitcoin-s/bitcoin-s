package org.bitcoins.server.routes

import akka.http.scaladsl.server.StandardRoute

trait ServerRoute {
  def handleCommand: PartialFunction[ServerCommand, StandardRoute]
}
