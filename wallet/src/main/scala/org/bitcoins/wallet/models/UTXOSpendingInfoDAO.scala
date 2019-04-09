package org.bitcoins.wallet.models

import org.bitcoins.db.{CRUD, CRUDAutoInc, DbConfig, TableAutoInc}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContext

case class UTXOSpendingInfoDAO(dbConfig: DbConfig)(
    implicit executionContext: ExecutionContext)
    extends CRUDAutoInc[UTXOSpendingInfoDb] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val ec: ExecutionContext = executionContext

  /** The table inside our database we are inserting into */
  override val table = TableQuery[UTXOSpendingInfoTable]
}
