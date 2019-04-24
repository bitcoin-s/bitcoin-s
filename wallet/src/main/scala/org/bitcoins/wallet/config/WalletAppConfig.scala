package org.bitcoins.wallet.config

import org.bitcoins.db.AppConfig
import org.bitcoins.wallet.db.WalletDbConfig

case class WalletAppConfig(dbConfig: WalletDbConfig) extends AppConfig[WalletDbConfig]
