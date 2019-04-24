package org.bitcoins.chain.db

import org.bitcoins.db._

sealed trait ChainDbConfig extends DbConfig {
  override val configPath: String = "chain.conf"
}

object ChainMainNetDbConfig extends ChainDbConfig {
  override val networkDb = NetworkDb.MainNetDbConfig
}

object ChainTestNet3DbConfig extends ChainDbConfig {
  override val networkDb = NetworkDb.TestNet3DbConfig
}

object ChainRegTestDbConfig extends ChainDbConfig  {
  override val networkDb = NetworkDb.RegTestDbConfig
}

/** It is useful for unit tests to specify what network we want to test against */
case class ChainUnitTestDbConfig(networkDb: NetworkDb) extends ChainDbConfig
