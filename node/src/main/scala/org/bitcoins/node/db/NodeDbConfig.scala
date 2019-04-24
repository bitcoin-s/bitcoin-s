package org.bitcoins.node.db

import org.bitcoins.chain.db._
import org.bitcoins.db._

sealed trait NodeDbConfig extends DbConfig {
  override val configPath: String = "node.conf"

  /** Gives us the corresponding [[org.bitcoins.chain.db.ChainDbConfig ChainDbConfig]]
    * to this NodeDbConfig. Useful when constructing things in the node project
    * that need to interact with chain
    * @return
    */
  def toChainDbConfig: ChainDbConfig = this match {
    case NodeMainNetDbConfig => ChainMainNetDbConfig
    case NodeTestNet3DbConfig => ChainTestNet3DbConfig
    case NodeRegTestDbConfig => ChainRegTestDbConfig
    case NodeUnitTestDbConfig(network) => ChainUnitTestDbConfig(network)
  }
}

object NodeMainNetDbConfig extends NodeDbConfig {
  override val networkDb = NetworkDb.MainNetDbConfig
}

object NodeTestNet3DbConfig extends NodeDbConfig {
  override val networkDb = NetworkDb.TestNet3DbConfig
}

object NodeRegTestDbConfig extends NodeDbConfig {
  override val networkDb = NetworkDb.RegTestDbConfig
}


/** It is useful for unit tests to specify what network we want to test against */
case class NodeUnitTestDbConfig(networkDb: NetworkDb) extends NodeDbConfig
