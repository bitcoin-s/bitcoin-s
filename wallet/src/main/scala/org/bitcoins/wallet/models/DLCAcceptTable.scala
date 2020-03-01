package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.dlc.DLCMessage.DLCAccept
import org.bitcoins.dlc.{CETSignatures, DLCPublicKeys}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

case class DLCAcceptDb(
    eventId: Sha256DigestBE,
    fundingKey: ECPublicKey,
    toLocalCETKey: ECPublicKey,
    finalAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    winSig: PartialSignature,
    loseSig: PartialSignature,
    refundSig: PartialSignature,
    changeAddress: BitcoinAddress) {

  def toDLCAccept(fundingInputs: Vector[OutputReference]): DLCAccept = {
    val pubKeys =
      DLCPublicKeys(fundingKey, toLocalCETKey, finalAddress)
    val cetSigs = CETSignatures(winSig, loseSig, refundSig)
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
      accept.cetSigs.winSig,
      accept.cetSigs.loseSig,
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

  def winSig: Rep[PartialSignature] = column("winSig")

  def loseSig: Rep[PartialSignature] = column("loseSig")

  def refundSig: Rep[PartialSignature] = column("refundSig")

  def changeAddress: Rep[BitcoinAddress] = column("changeAddress")

  private type DLCTuple = (
      Sha256DigestBE,
      ECPublicKey,
      ECPublicKey,
      BitcoinAddress,
      CurrencyUnit,
      PartialSignature,
      PartialSignature,
      PartialSignature,
      BitcoinAddress)

  private val fromTuple: DLCTuple => DLCAcceptDb = {
    case (eventId,
          fundingKey,
          toLocalCETKey,
          finalAddress,
          totalCollateral,
          winSig,
          loseSig,
          refundSig,
          changeAddress) =>
      DLCAcceptDb(eventId,
                  fundingKey,
                  toLocalCETKey,
                  finalAddress,
                  totalCollateral,
                  winSig,
                  loseSig,
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
       dlc.winSig,
       dlc.loseSig,
       dlc.refundSig,
       dlc.changeAddress))

  def * : ProvenShape[DLCAcceptDb] =
    (eventId,
     fundingKey,
     toLocalCETKey,
     finalAddress,
     totalCollateral,
     winSig,
     loseSig,
     refundSig,
     changeAddress) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_dlc", sourceColumns = eventId)

  def fk: ForeignKeyQuery[DLCTable, DLCDb] =
    foreignKey("fk_eventId",
               sourceColumns = eventId,
               targetTableQuery = TableQuery[DLCTable])(_.eventId)
}
