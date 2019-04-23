package org.bitcoins.chain.db

import org.bitcoins.db._

trait ChainDbConfig extends DbConfig { this: NetworkDb =>
  override val configPath: String = "chain.conf"
}

object ChainMainNetDbConfig extends MainNetDbConfig with ChainDbConfig

object ChainTestNet3DbConfig extends TestNet3DbConfig with ChainDbConfig

object ChainRegTestDbConfig extends RegTestDbConfig with ChainDbConfig

object ChainUnitTestDbConfig extends UnitTestDbConfig with ChainDbConfig
