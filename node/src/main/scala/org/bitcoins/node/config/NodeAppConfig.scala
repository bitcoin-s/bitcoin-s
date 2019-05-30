package org.bitcoins.node.config

import com.typesafe.config.Config
import org.bitcoins.db.AppConfig
import org.bitcoins.node.db.NodeDbManagement

import scala.concurrent.{ExecutionContext, Future}

case class NodeAppConfig(confs: Config*) extends AppConfig {
  override val configOverrides: List[Config] = confs.toList
  override protected def moduleConfigName: String = "node.conf"
  override protected type ConfigType = NodeAppConfig
  override protected def newConfigOfType(configs: List[Config]): NodeAppConfig =
    NodeAppConfig(configs: _*)

  val bloomFalsePositiveRate: Double =
    config.getDouble("node.bloomFalsePositiveRate")

  /**
    * Ensures correct tables and other required information is in
    * place for our node.
    */
  def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    NodeDbManagement.createAll()(config = this, ec)
  }

}
