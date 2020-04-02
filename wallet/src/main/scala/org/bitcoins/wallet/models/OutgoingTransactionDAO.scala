package org.bitcoins.wallet.models

import org.bitcoins.wallet.config._
import slick.lifted.TableQuery

import scala.concurrent.ExecutionContext

case class OutgoingTransactionDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends TxDAO[OutgoingTransactionDb, OutgoingTransactionTable] {
  override val table = TableQuery[OutgoingTransactionTable]
}
