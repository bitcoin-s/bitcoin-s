package org.bitcoins.wallet.db

import org.bitcoins.db._

sealed trait WalletDbConfig extends DbConfig { this : NetworkDb =>
  override val configPath: String = "wallet.conf"
}

object WalletMainNetDbConfig extends  WalletDbConfig with MainNetDbConfig

object WalletTestNet3DbConfig extends WalletDbConfig with TestNet3DbConfig

object WalletRegTestDbConfig extends WalletDbConfig with RegTestDbConfig

object WalletUnitTestDbConfig extends WalletDbConfig with UnitTestDbConfig
