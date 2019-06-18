package org.bitcoins.wallet.models

import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContext

case class UTXOSpendingInfoDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUDAutoInc[UTXOSpendingInfoDb] {

  /** The table inside our database we are inserting into */
  override val table = TableQuery[UTXOSpendingInfoTable]
}
