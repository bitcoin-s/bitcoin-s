package org.bitcoins.node.models

import org.bitcoins.db.{CRUDAutoInc}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContext
import org.bitcoins.node.config.NodeAppConfig

case class PeerDAO()(implicit ec: ExecutionContext, appConfig: NodeAppConfig)
    extends CRUDAutoInc[Peer] {
  override val table = TableQuery[PeerTable]
}
