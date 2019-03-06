package org.bitcoins.chain.config

import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  MainNetChainParams,
  RegTestNetChainParams,
  TestNetChainParams
}
import org.bitcoins.db.{
  DbConfig,
  MainNetDbConfig,
  RegTestDbConfig,
  TestNet3DbConfig
}

import slick.jdbc.SQLiteProfile.api._

sealed abstract class ChainConfig {

  def networkParameters: NetworkParameters = RegTest

  /** This is the configuration details needed to connect to our database */
  def dbConfig: DbConfig = networkParameters match {
    case MainNet  => MainNetDbConfig
    case TestNet3 => TestNet3DbConfig
    case RegTest  => RegTestDbConfig
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

object ChainConfig extends ChainConfig
