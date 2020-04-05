package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

case class PTLCFundingInputDb(
    invoiceId: Sha256DigestBE,
    outPoint: TransactionOutPoint) {}

class PTLCFundingInputsTable(tag: Tag)
    extends Table[PTLCFundingInputDb](tag, "wallet_ptlc_funding_inputs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def invoiceId: Rep[Sha256DigestBE] = column("invoiceId")

  def outPoint: Rep[TransactionOutPoint] = column("outPoint", O.Unique)

  private type PTLCFundingInputTuple = (Sha256DigestBE, TransactionOutPoint)

  private val fromTuple: PTLCFundingInputTuple => PTLCFundingInputDb = {
    case (invoiceId, outPoint) =>
      PTLCFundingInputDb(invoiceId, outPoint)
  }

  private val toTuple: PTLCFundingInputDb => Option[PTLCFundingInputTuple] =
    ptlc => Some((ptlc.invoiceId, ptlc.outPoint))

  def * : ProvenShape[PTLCFundingInputDb] =
    (invoiceId, outPoint) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_ptlcInput", sourceColumns = outPoint)

  def fk: ForeignKeyQuery[PTLCTable, PTLCDb] =
    foreignKey("fk_invoiceId",
               sourceColumns = invoiceId,
               targetTableQuery = TableQuery[PTLCTable])(_.invoiceId)
}
