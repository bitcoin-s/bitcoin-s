package org.bitcoins.wallet.models

import org.bitcoins.wallet.config._
import slick.lifted.TableQuery

import scala.concurrent.ExecutionContext

case class IncomingTransactionDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends TxDAO[IncomingTransactionDb, IncomingTransactionTable] {
  override val table = TableQuery[IncomingTransactionTable]
}
