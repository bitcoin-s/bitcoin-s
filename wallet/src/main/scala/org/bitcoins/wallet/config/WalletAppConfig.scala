package org.bitcoins.wallet.config

import org.bitcoins.db.AppConfig

case object WalletAppConfig extends AppConfig {
  override def moduleConfigName: String = "wallet.conf"
}
