package org.bitcoins.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.DLCAccept
import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, DLCPublicKeys}
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

case class DLCAcceptDb(
    eventId: Sha256DigestBE,
    fundingKey: ECPublicKey,
    toLocalCETKey: ECPublicKey,
    finalAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    refundSig: PartialSignature,
    changeAddress: BitcoinAddress) {

  def toDLCAccept(
      fundingInputs: Vector[OutputReference],
      outcomeSigs: Map[Sha256DigestBE, PartialSignature]): DLCAccept = {
    val pubKeys =
      DLCPublicKeys(fundingKey, toLocalCETKey, finalAddress)
    val cetSigs = CETSignatures(outcomeSigs, refundSig)
    DLCAccept(totalCollateral.satoshis,
              pubKeys,
              fundingInputs,
              changeAddress,
              cetSigs,
              eventId)
  }
}

object DLCAcceptDb {

  def fromDLCAccept(accept: DLCAccept): DLCAcceptDb = {
    DLCAcceptDb(
      accept.eventId,
      accept.pubKeys.fundingKey,
      accept.pubKeys.toLocalCETKey,
      accept.pubKeys.finalAddress,
      accept.totalCollateral,
      accept.cetSigs.refundSig,
      accept.changeAddress
    )
  }
}

class DLCAcceptTable(tag: Tag)
    extends Table[DLCAcceptDb](tag, "wallet_dlc_accepts") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

  def fundingKey: Rep[ECPublicKey] = column("fundingKey")

  def toLocalCETKey: Rep[ECPublicKey] = column("toLocalCETKey")

  def finalAddress: Rep[BitcoinAddress] = column("finalAddress")

  def totalCollateral: Rep[CurrencyUnit] = column("totalCollateral")

  def refundSig: Rep[PartialSignature] = column("refundSig")

  def changeAddress: Rep[BitcoinAddress] = column("changeAddress")

  private type DLCTuple = (
      Sha256DigestBE,
      ECPublicKey,
      ECPublicKey,
      BitcoinAddress,
      CurrencyUnit,
      PartialSignature,
      BitcoinAddress)

  private val fromTuple: DLCTuple => DLCAcceptDb = {
    case (eventId,
          fundingKey,
          toLocalCETKey,
          finalAddress,
          totalCollateral,
          refundSig,
          changeAddress) =>
      DLCAcceptDb(eventId,
                  fundingKey,
                  toLocalCETKey,
                  finalAddress,
                  totalCollateral,
                  refundSig,
                  changeAddress)
  }

  private val toTuple: DLCAcceptDb => Option[DLCTuple] = dlc =>
    Some(
      (dlc.eventId,
       dlc.fundingKey,
       dlc.toLocalCETKey,
       dlc.finalAddress,
       dlc.totalCollateral,
       dlc.refundSig,
       dlc.changeAddress))

  def * : ProvenShape[DLCAcceptDb] =
    (eventId,
     fundingKey,
     toLocalCETKey,
     finalAddress,
     totalCollateral,
     refundSig,
     changeAddress) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_dlc", sourceColumns = eventId)

  def fk: ForeignKeyQuery[DLCTable, DLCDb] =
    foreignKey("fk_eventId",
               sourceColumns = eventId,
               targetTableQuery = TableQuery[DLCTable])(_.eventId)
}
