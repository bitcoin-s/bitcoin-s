package org.bitcoins.wallet.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig

case class WalletAppConfig(conf: Config*) extends AppConfig {
  override val configOverrides: List[Config] = conf.toList
  override def moduleConfigName: String = "wallet.conf"
  override type ConfigType = WalletAppConfig
  override def newConfigOfType(configs: List[Config]): WalletAppConfig =
    WalletAppConfig(configs: _*)
}
