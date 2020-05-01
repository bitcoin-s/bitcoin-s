package org.bitcoins.wallet.db

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models._

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
  private lazy val dlcTable: TableQuery[Table[_]] = {
    DLCDAO()(ec, appConfig).table
  }
  private lazy val dlcOfferTable: TableQuery[Table[_]] = {
    DLCOfferDAO()(ec, appConfig).table
  }
  private lazy val dlcAcceptTable: TableQuery[Table[_]] = {
    DLCAcceptDAO()(ec, appConfig).table
  }
  private lazy val dlcFundingInputsTable: TableQuery[Table[_]] = {
    DLCFundingInputDAO()(ec, appConfig).table
  }
  private lazy val dlcCETSigTable: TableQuery[Table[_]] = {
    DLCCETSignatureDAO()(ec, appConfig).table
  }

  override lazy val allTables: List[TableQuery[Table[_]]] = {

    List(
      accountTable,
      addressTable,
      utxoTable,
      txTable,
      incomingTxTable,
      outgoingTxTable,
      dlcTable,
      dlcOfferTable,
      dlcAcceptTable,
      dlcFundingInputsTable,
      dlcCETSigTable
    )
  }

}
