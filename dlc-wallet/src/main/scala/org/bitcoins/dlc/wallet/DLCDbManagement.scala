package org.bitcoins.dlc.wallet

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.dlc.wallet.models._

import scala.concurrent.ExecutionContext

trait DLCDbManagement extends DbManagement {
  self: JdbcProfileComponent[DLCAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val announcementTable: TableQuery[Table[?]] = {
    OracleAnnouncementDataDAO()(using ec, appConfig).table
  }

  private lazy val nonceTable: TableQuery[Table[?]] = {
    OracleNonceDAO()(using ec, appConfig).table
  }

  private lazy val dlcTable: TableQuery[Table[?]] = {
    DLCDAO()(using ec, appConfig).table
  }

  private lazy val contractDataTable: TableQuery[Table[?]] = {
    DLCContractDataDAO()(using ec, appConfig).table
  }

  private lazy val dlcAnnouncementTable: TableQuery[Table[?]] = {
    DLCAnnouncementDAO()(using ec, appConfig).table
  }

  private lazy val dlcOfferTable: TableQuery[Table[?]] = {
    DLCOfferDAO()(using ec, appConfig).table
  }

  private lazy val dlcAcceptTable: TableQuery[Table[?]] = {
    DLCAcceptDAO()(using ec, appConfig).table
  }

  private lazy val dlcFundingInputsTable: TableQuery[Table[?]] = {
    DLCFundingInputDAO()(using ec, appConfig).table
  }

  private lazy val dlcCETSigTable: TableQuery[Table[?]] = {
    DLCCETSignaturesDAO()(using ec, appConfig).table
  }

  private lazy val dlcRefundSigTable: TableQuery[Table[?]] = {
    DLCRefundSigsDAO()(using ec, appConfig).table
  }

  private lazy val dlcRemoteTxTable: TableQuery[Table[?]] = {
    DLCRemoteTxDAO()(using ec, appConfig).table
  }

  // Ordering matters here, tables with a foreign key should be listed after
  // the table that key references
  override lazy val allTables: List[TableQuery[Table[?]]] = {
    List(
      announcementTable,
      nonceTable,
      dlcTable,
      contractDataTable,
      dlcAnnouncementTable,
      dlcOfferTable,
      dlcAcceptTable,
      dlcFundingInputsTable,
      dlcCETSigTable,
      dlcRefundSigTable,
      dlcRemoteTxTable
    )
  }

}
