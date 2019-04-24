package org.bitcoins.wallet.db

import org.bitcoins.db._

sealed trait WalletDbConfig extends DbConfig {
  override val configPath: String = "wallet.conf"
}

object WalletMainNetDbConfig extends  WalletDbConfig {
  override val networkDb: NetworkDb.MainNetDbConfig.type = NetworkDb.MainNetDbConfig
}

object WalletTestNet3DbConfig extends WalletDbConfig {
  override val networkDb: NetworkDb.TestNet3DbConfig.type = NetworkDb.TestNet3DbConfig
}


object WalletRegTestDbConfig extends WalletDbConfig {
  override val networkDb: NetworkDb.RegTestDbConfig.type = NetworkDb.RegTestDbConfig
}


/** It is useful for unit tests to specify what network we want to test against */
case class WalletUnitTestDbConfig(networkDb: NetworkDb) extends WalletDbConfig
