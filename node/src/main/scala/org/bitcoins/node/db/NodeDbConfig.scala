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
    case NodeDbConfig.MainNetDbConfig => ChainDbConfig.MainNetDbConfig
    case NodeDbConfig.TestNet3DbConfig => ChainDbConfig.TestNet3DbConfig
    case NodeDbConfig.RegTestDbConfig => ChainDbConfig.RegTestDbConfig
    case NodeDbConfig.UnitTestDbConfig(network) => ChainDbConfig.UnitTestDbConfig(network)
  }
}

object NodeDbConfig {
  final case object MainNetDbConfig extends NodeDbConfig {
    override val networkDb = NetworkDb.MainNetDbConfig
  }

  final case object TestNet3DbConfig extends NodeDbConfig {
    override val networkDb = NetworkDb.TestNet3DbConfig
  }

  final case object RegTestDbConfig extends NodeDbConfig {
    override val networkDb = NetworkDb.RegTestDbConfig
  }

  /** It is useful for unit tests to specify what network we want to test against */
  case class UnitTestDbConfig(networkDb: NetworkDb) extends NodeDbConfig

}


