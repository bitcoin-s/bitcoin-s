package org.bitcoins.server.util

import akka.actor.ActorSystem

trait BitcoinSApp extends App {
  def actorSystemName: String

  implicit lazy val system: ActorSystem = ActorSystem(actorSystemName)
}
