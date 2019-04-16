package org.bitcoins.wallet.db

import org.bitcoins.db._

sealed trait WalletDbConfig { this: DbConfig =>
  override val configPath: String = "wallet.conf"
}

object WalletMainNetDbConfig extends MainNetDbConfig  with WalletDbConfig

object WalletTestNet3DbConfig extends TestNet3DbConfig with WalletDbConfig

object WalletRegTestDbConfig extends RegTestDbConfig with WalletDbConfig

object WalletUnitTestDbConfig extends UnitTestDbConfig with WalletDbConfig
