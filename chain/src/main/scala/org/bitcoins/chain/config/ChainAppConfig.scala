package org.bitcoins.chain.config

import org.bitcoins.chain.db.ChainDbConfig
import org.bitcoins.db.AppConfig

case class ChainAppConfig(dbConfig: ChainDbConfig) extends AppConfig[ChainDbConfig]
