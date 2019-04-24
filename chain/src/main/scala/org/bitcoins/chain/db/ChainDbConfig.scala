package org.bitcoins.chain.db

import org.bitcoins.db._

sealed trait ChainDbConfig extends DbConfig {
  override val configPath: String = "chain.conf"
}


object ChainDbConfig {
  final case object MainNetDbConfig extends ChainDbConfig {
    override val networkDb = NetworkDb.MainNetDbConfig
  }

  final case object TestNet3DbConfig extends ChainDbConfig {
    override val networkDb = NetworkDb.TestNet3DbConfig
  }

  final case object RegTestDbConfig extends ChainDbConfig  {
    override val networkDb = NetworkDb.RegTestDbConfig
  }

  /** It is useful for unit tests to specify what network we want to test against */
  case class UnitTestDbConfig(networkDb: NetworkDb) extends ChainDbConfig
}

