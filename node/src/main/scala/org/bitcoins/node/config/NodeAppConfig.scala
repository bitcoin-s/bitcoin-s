package org.bitcoins.node.config

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.db.AppConfig
import org.bitcoins.node.db.NodeDbConfig

case class NodeAppConfig(dbConfig: NodeDbConfig) extends AppConfig[NodeDbConfig] {

  def chainAppConfig: ChainAppConfig = {
    ChainAppConfig(dbConfig = dbConfig.toChainDbConfig)
  }
}
