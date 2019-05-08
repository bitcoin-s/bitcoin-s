package org.bitcoins.node.models

import org.bitcoins.db.{CRUDAutoInc}
import slick.jdbc.SQLiteProfile.api._

import org.bitcoins.db.AppConfig
import scala.concurrent.ExecutionContext

case class PeerDAO(appConfig: AppConfig)(
    implicit override val ec: ExecutionContext)
    extends CRUDAutoInc[Peer] {
  override val table = TableQuery[PeerTable]
}
