package org.bitcoins.node.constant

import akka.actor.ActorSystem
import org.bitcoins.node.versions.ProtocolVersion70013

import scala.concurrent.duration.DurationInt
import com.typesafe.config.ConfigFactory

case object Constants {
  val emptyConfig = ConfigFactory.parseString("")
  lazy val actorSystem = ActorSystem("BitcoinSpvNode", emptyConfig)
  def version = ProtocolVersion70013

  def timeout = 5.seconds
  def userAgent = "/bitcoins-spv-node/0.0.1"

  /** This is the file where our block headers are stored */
  def blockHeaderFile = new java.io.File("src/main/resources/block_headers.dat")

}
