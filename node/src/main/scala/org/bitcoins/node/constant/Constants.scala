package org.bitcoins.node.constant

import akka.actor.ActorSystem
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.blockchain.{ChainParams, MainNetChainParams, RegTestNetChainParams, TestNetChainParams}
import org.bitcoins.db.DbConfig
import org.bitcoins.node.db._
import org.bitcoins.node.versions.ProtocolVersion70013
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.duration.DurationInt

/**
  * Created by chris on 7/1/16.
  */
sealed abstract class Constants {
  lazy val actorSystem = ActorSystem("BitcoinSpvNode")
  def networkParameters: NetworkParameters = TestNet3
  def version = ProtocolVersion70013

  def timeout = 5.seconds
  def userAgent = "/bitcoins-spv-node/0.0.1"

  /** This is the file where our block headers are stored */
  def blockHeaderFile = new java.io.File("src/main/resources/block_headers.dat")

  /** This is the configuration details needed to connect to our database */
  def dbConfig: DbConfig = networkParameters match {
    case MainNet  => NodeDbConfig.MainNetDbConfig
    case TestNet3 => NodeDbConfig.TestNet3DbConfig
    case RegTest  => NodeDbConfig.RegTestDbConfig
  }

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
  def database: Database = dbConfig.database
}

object Constants extends Constants
