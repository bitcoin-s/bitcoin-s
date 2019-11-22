package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

case class DLCFundingInputDb(
    eventId: Sha256DigestBE,
    isInitiator: Boolean,
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    sigs: Vector[PartialSignature]) {

  def toOutputReference: OutputReference =
    OutputReference(outPoint, output)
}

class DLCFundingInputsTable(tag: Tag)
    extends Table[DLCFundingInputDb](tag, "wallet_dlc_funding_inputs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def eventId: Rep[Sha256DigestBE] = column("eventId")

  def isInitiator: Rep[Boolean] = column("isInitiator")

  def outPoint: Rep[TransactionOutPoint] = column("outPoint", O.Unique)

  def output: Rep[TransactionOutput] = column("output")

  def sigs: Rep[Vector[PartialSignature]] = column("sigs")

  private type DLCTuple = (
      Sha256DigestBE,
      Boolean,
      TransactionOutPoint,
      TransactionOutput,
      Vector[PartialSignature])

  private val fromTuple: DLCTuple => DLCFundingInputDb = {
    case (eventId, isInitiator, outPoint, output, sigs) =>
      DLCFundingInputDb(eventId, isInitiator, outPoint, output, sigs)
  }

  private val toTuple: DLCFundingInputDb => Option[DLCTuple] = dlc =>
    Some((dlc.eventId, dlc.isInitiator, dlc.outPoint, dlc.output, dlc.sigs))

  def * : ProvenShape[DLCFundingInputDb] =
    (eventId, isInitiator, outPoint, output, sigs) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_dlcInput", sourceColumns = outPoint)

  def fk: ForeignKeyQuery[DLCTable, DLCDb] =
    foreignKey("fk_eventId",
               sourceColumns = eventId,
               targetTableQuery = TableQuery[DLCTable])(_.eventId)
}
