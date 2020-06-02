package org.bitcoins.wallet.db

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AccountDAO,
  AddressDAO,
  IncomingTransactionDAO,
  OutgoingTransactionDAO,
  SpendingInfoDAO,
  TransactionDAO
}

import scala.concurrent.ExecutionContext

trait WalletDbManagement extends DbManagement {
  _: JdbcProfileComponent[WalletAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val accountTable: TableQuery[Table[_]] = {
    AccountDAO()(ec, appConfig).table
  }
  private lazy val addressTable: TableQuery[Table[_]] = {
    AddressDAO()(ec, appConfig).table
  }
  private lazy val utxoTable: TableQuery[Table[_]] = {
    SpendingInfoDAO()(ec, appConfig).table
  }
  private lazy val txTable: TableQuery[Table[_]] = {
    TransactionDAO()(ec, appConfig).table
  }
  private lazy val incomingTxTable: TableQuery[Table[_]] = {
    IncomingTransactionDAO()(ec, appConfig).table
  }
  private lazy val outgoingTxTable: TableQuery[Table[_]] = {
    OutgoingTransactionDAO()(ec, appConfig).table
  }

  override lazy val allTables: List[TableQuery[Table[_]]] = {
    List(accountTable,
         addressTable,
         txTable,
         incomingTxTable,
         utxoTable,
         outgoingTxTable)
  }

}
