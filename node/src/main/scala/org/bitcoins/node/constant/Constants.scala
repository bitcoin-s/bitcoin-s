package org.bitcoins.node.constant

import akka.actor.ActorSystem
import scala.concurrent.duration.DurationInt

case object Constants {
  lazy val actorSystem = ActorSystem("BitcoinSpvNode")

  def timeout = 5.seconds

  /** This is the file where our block headers are stored */
  def blockHeaderFile = new java.io.File("src/main/resources/block_headers.dat")
}
