package org.bitcoins.node.models

import org.bitcoins.db.{CRUDAutoInc, DbConfig}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContext

abstract class PeerDAO extends CRUDAutoInc[Peer] {
  override val table = TableQuery[PeerTable]
}

object PeerDAO {
  private case class PeerDAOImpl(dbConfig: DbConfig)(
      override implicit val ec: ExecutionContext)
      extends PeerDAO

  def apply(dbConfig: DbConfig)(implicit ec: ExecutionContext): PeerDAO = {
    PeerDAOImpl(dbConfig)
  }
}
