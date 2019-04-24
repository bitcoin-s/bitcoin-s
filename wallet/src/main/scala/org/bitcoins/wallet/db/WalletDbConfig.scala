package org.bitcoins.wallet.db

import org.bitcoins.db._

sealed trait WalletDbConfig extends DbConfig {
  override val configPath: String = "wallet.conf"
}
object WalletDbConfig {
  final case object MainNetDbConfig extends  WalletDbConfig {
    override val networkDb: NetworkDb.MainNetDbConfig.type = NetworkDb.MainNetDbConfig
  }

  final case object TestNet3DbConfig extends WalletDbConfig {
    override val networkDb: NetworkDb.TestNet3DbConfig.type = NetworkDb.TestNet3DbConfig
  }


  final case object RegTestDbConfig extends WalletDbConfig {
    override val networkDb: NetworkDb.RegTestDbConfig.type = NetworkDb.RegTestDbConfig
  }

  /** It is useful for unit tests to specify what network we want to test against */
  case class UnitTestDbConfig(networkDb: NetworkDb) extends WalletDbConfig

}
