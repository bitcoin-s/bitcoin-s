package org.bitcoins.node.config

import org.bitcoins.db.AppConfig

case object NodeAppConfig extends AppConfig {
  override def moduleConfigName: String = "node.conf"
}
