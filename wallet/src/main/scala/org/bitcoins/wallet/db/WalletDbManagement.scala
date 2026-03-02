package org.bitcoins.wallet.db

import org.bitcoins.db.models.MasterXPubDAO
import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._

import scala.concurrent.ExecutionContext

trait WalletDbManagement extends DbManagement {
  this: JdbcProfileComponent[WalletAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val accountTable: TableQuery[Table[?]] = {
    AccountDAO()(using ec, appConfig).table
  }

  private lazy val addressTable: TableQuery[Table[?]] = {
    AddressDAO()(using ec, appConfig).table
  }

  private lazy val addressTagTable: TableQuery[Table[?]] = {
    AddressTagDAO()(using ec, appConfig).table
  }

  private lazy val utxoTable: TableQuery[Table[?]] = {
    SpendingInfoDAO()(using ec, appConfig).table
  }

  private lazy val txTable: TableQuery[Table[?]] = {
    TransactionDAO()(using ec, appConfig).table
  }

  private lazy val incomingTxTable: TableQuery[Table[?]] = {
    IncomingTransactionDAO()(using ec, appConfig).table
  }

  private lazy val outgoingTxTable: TableQuery[Table[?]] = {
    OutgoingTransactionDAO()(using ec, appConfig).table
  }

  private lazy val spkTable: TableQuery[Table[?]] = {
    ScriptPubKeyDAO()(using ec, appConfig).table
  }

  private lazy val stateDescriptorTable: TableQuery[Table[?]] = {
    WalletStateDescriptorDAO()(using ec, appConfig).table
  }

  private lazy val masterXPubTable: TableQuery[Table[?]] = {
    MasterXPubDAO()(using ec, appConfig).table
  }

  // Ordering matters here, tables with a foreign key should be listed after
  // the table that key references
  override lazy val allTables: List[TableQuery[Table[?]]] = {
    List(
      spkTable,
      accountTable,
      addressTable,
      addressTagTable,
      txTable,
      incomingTxTable,
      utxoTable,
      outgoingTxTable,
      stateDescriptorTable,
      masterXPubTable
    )
  }

}
