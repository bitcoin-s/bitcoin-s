package org.bitcoins.server.util

import akka.actor.ActorSystem

trait BitcoinSApp extends App {
  def actorSystemName: String // = "bitcoin-s-server"

  implicit lazy val system: ActorSystem = ActorSystem(actorSystemName)
}
