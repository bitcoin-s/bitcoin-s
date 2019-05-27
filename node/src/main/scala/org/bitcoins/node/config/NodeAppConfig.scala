package org.bitcoins.node.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig

case class NodeAppConfig(
    override val config: Config = AppConfig.defaultNodeConfig)
    extends AppConfig {
  override def moduleConfigName: String = "node.conf"
}
