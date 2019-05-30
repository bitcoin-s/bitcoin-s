package org.bitcoins.node.constant

import akka.actor.ActorSystem
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  MainNetChainParams,
  RegTestNetChainParams,
  TestNetChainParams
}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.versions.ProtocolVersion70013
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.duration.DurationInt

case object Constants {
  lazy val actorSystem = ActorSystem("BitcoinSpvNode")
  def networkParameters: NetworkParameters = appConfig.network
  def version = ProtocolVersion70013

  def timeout = 5.seconds
  def userAgent = "/bitcoins-spv-node/0.0.1"

  /** This is the file where our block headers are stored */
  def blockHeaderFile = new java.io.File("src/main/resources/block_headers.dat")

  lazy val appConfig: NodeAppConfig = NodeAppConfig()

  /** The [[ChainParams]] for the blockchain we are currently connected to */
  def chainParams: ChainParams = networkParameters match {
    case MainNet  => MainNetChainParams
    case TestNet3 => TestNetChainParams
    case RegTest  => RegTestNetChainParams
  }

  /** This is the database we are currently bound to, this
    * should be the database that stores information corresponding to the network
    * we are currently connected to inside of the [[networkParameters]] function
    * @return
    */
  def database: Database = appConfig.database
}
