package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

case class DLCCETSignatureDb(
    eventId: Sha256DigestBE,
    outcomeHash: Sha256DigestBE,
    signature: PartialSignature) {
  def toTuple: (Sha256DigestBE, PartialSignature) = (outcomeHash, signature)
}

class DLCCETSignatureTable(tag: Tag)
    extends Table[DLCCETSignatureDb](tag, "wallet_dlc_cet_sigs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def eventId: Rep[Sha256DigestBE] = column("eventId")

  def outcomeHash: Rep[Sha256DigestBE] = column("outcomeHash")

  def signature: Rep[PartialSignature] = column("signature")

  private type DLCCETSignatureTuple =
    (Sha256DigestBE, Sha256DigestBE, PartialSignature)

  private val fromTuple: DLCCETSignatureTuple => DLCCETSignatureDb = {
    case (eventId, outcomeHash, signature) =>
      DLCCETSignatureDb(
        eventId,
        outcomeHash,
        signature
      )
  }

  private val toTuple: DLCCETSignatureDb => Option[DLCCETSignatureTuple] =
    dlc => Some((dlc.eventId, dlc.outcomeHash, dlc.signature))

  def * : ProvenShape[DLCCETSignatureDb] =
    (eventId, outcomeHash, signature) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_dlc", sourceColumns = (eventId, outcomeHash))

  def fk: ForeignKeyQuery[DLCTable, DLCDb] =
    foreignKey("fk_eventId",
               sourceColumns = eventId,
               targetTableQuery = TableQuery[DLCTable])(_.eventId)
}
