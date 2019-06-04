package org.bitcoins.wallet.models

import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.bitcoins.db.AppConfig

case class UTXOSpendingInfoDAO()(implicit val ec: ExecutionContext)
    extends CRUDAutoInc[UTXOSpendingInfoDb] {

  override def appConfig: WalletAppConfig = WalletAppConfig()

  /** The table inside our database we are inserting into */
  override val table = TableQuery[UTXOSpendingInfoTable]

  def findAllUTXOs(): Future[Vector[UTXOSpendingInfoDb]] =
    database.run(table.result).map(_.toVector)
}
