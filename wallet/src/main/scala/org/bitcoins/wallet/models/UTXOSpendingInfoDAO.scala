package org.bitcoins.wallet.models

import org.bitcoins.db.{CRUDAutoInc, DbConfig}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class UTXOSpendingInfoDAO(dbConfig: DbConfig)(
    implicit val ec: ExecutionContext)
    extends CRUDAutoInc[UTXOSpendingInfoDb] {

  /** The table inside our database we are inserting into */
  override val table = TableQuery[UTXOSpendingInfoTable]

  def findAllUTXOs(): Future[Vector[UTXOSpendingInfoDb]] =
    database.run(table.result).map(_.toVector)
}
