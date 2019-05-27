package org.bitcoins.wallet.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig

case class WalletAppConfig(
    override val config: Config = AppConfig.defaultWalletConfig)
    extends AppConfig {
  override def moduleConfigName: String = "wallet.conf"
}
