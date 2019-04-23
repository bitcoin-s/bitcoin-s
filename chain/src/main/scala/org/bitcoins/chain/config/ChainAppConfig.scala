package org.bitcoins.chain.config

import org.bitcoins.chain.db.ChainDbConfig
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.db.AppConfig

case class ChainAppConfig(dbConfig: ChainDbConfig, chain: ChainParams) extends AppConfig[ChainDbConfig]
