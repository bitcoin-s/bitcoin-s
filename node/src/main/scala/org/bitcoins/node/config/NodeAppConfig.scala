package org.bitcoins.node.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig

case class NodeAppConfig(confs: Config*) extends AppConfig {
  override val configOverrides: List[Config] = confs.toList
  override protected def moduleConfigName: String = "node.conf"
  override protected type ConfigType = NodeAppConfig
  override protected def newConfigOfType(configs: List[Config]): NodeAppConfig =
    NodeAppConfig(configs: _*)

}
