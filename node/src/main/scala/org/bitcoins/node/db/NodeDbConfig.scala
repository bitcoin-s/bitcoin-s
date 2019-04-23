package org.bitcoins.node.db

import org.bitcoins.chain.db.{ChainDbConfig, ChainMainNetDbConfig, ChainRegTestDbConfig, ChainTestNet3DbConfig, ChainUnitTestDbConfig}
import org.bitcoins.db._

sealed trait NodeDbConfig extends DbConfig { this: NetworkDb =>
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
    case NodeUnitTestDbConfig => ChainUnitTestDbConfig
  }
}

object NodeMainNetDbConfig extends NodeDbConfig  with MainNetDbConfig

object NodeTestNet3DbConfig extends NodeDbConfig  with TestNet3DbConfig

object NodeRegTestDbConfig extends NodeDbConfig  with RegTestDbConfig

object NodeUnitTestDbConfig extends NodeDbConfig  with UnitTestDbConfig
