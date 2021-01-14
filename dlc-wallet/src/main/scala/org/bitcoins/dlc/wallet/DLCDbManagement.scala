package org.bitcoins.dlc.wallet

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.dlc.wallet.models._

import scala.concurrent.ExecutionContext

trait DLCDbManagement extends DbManagement {
  _: JdbcProfileComponent[DLCAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val announcementTable: TableQuery[Table[_]] = {
    OracleAnnouncementDAO()(ec, appConfig).table
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

  private lazy val dlcRefundSigTable: TableQuery[Table[_]] = {
    DLCRefundSigDAO()(ec, appConfig).table
  }

  private lazy val dlcRemoteTxTable: TableQuery[Table[_]] = {
    DLCRemoteTxDAO()(ec, appConfig).table
  }

  // Ordering matters here, tables with a foreign key should be listed after
  // the table that key references
  override lazy val allTables: List[TableQuery[Table[_]]] = {
    List(
      announcementTable,
      dlcTable,
      dlcOfferTable,
      dlcAcceptTable,
      dlcFundingInputsTable,
      dlcCETSigTable,
      dlcRefundSigTable,
      dlcRemoteTxTable
    )
  }

}
