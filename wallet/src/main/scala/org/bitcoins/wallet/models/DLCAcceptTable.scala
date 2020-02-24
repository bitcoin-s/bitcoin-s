package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.dlc.DLCMessage.DLCAccept
import org.bitcoins.dlc.{CETSignatures, DLCPublicKeys}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

case class DLCAcceptDb(
    eventId: Sha256DigestBE,
    pubKeys: DLCPublicKeys,
    totalCollateral: CurrencyUnit,
    fundingInputs: Vector[OutputReference],
    cetSigs: CETSignatures,
    changeAddress: Bech32Address) {

  def toDLCAccept: DLCAccept = {
    DLCAccept(totalCollateral.satoshis,
              pubKeys,
              fundingInputs,
              changeAddress,
              cetSigs,
              eventId)
  }
}

class DLCAcceptTable(tag: Tag)
    extends Table[DLCAcceptDb](tag, "wallet_dlc_accepts") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

  def pubKeys: Rep[DLCPublicKeys] = column("pubKeys")

  def totalCollateral: Rep[CurrencyUnit] = column("totalCollateral")

  def fundingInputs: Rep[Vector[OutputReference]] =
    column("fundingInputs")

  def cetSigs: Rep[CETSignatures] = column("cetSigs")

  def changeAddress: Rep[Bech32Address] = column("changeAddress")

  private type DLCTuple = (
      Sha256DigestBE,
      DLCPublicKeys,
      CurrencyUnit,
      Vector[OutputReference],
      CETSignatures,
      Bech32Address)

  private val fromTuple: DLCTuple => DLCAcceptDb = {
    case (eventId,
          pubKeys,
          totalCollateral,
          fundingInputs,
          cetSigs,
          changeAddress) =>
      DLCAcceptDb(eventId,
                  pubKeys,
                  totalCollateral,
                  fundingInputs,
                  cetSigs,
                  changeAddress)
  }

  private val toTuple: DLCAcceptDb => Option[DLCTuple] = dlc =>
    Some(
      (dlc.eventId,
       dlc.pubKeys,
       dlc.totalCollateral,
       dlc.fundingInputs,
       dlc.cetSigs,
       dlc.changeAddress))

  def * : ProvenShape[DLCAcceptDb] =
    (eventId, pubKeys, totalCollateral, fundingInputs, cetSigs, changeAddress) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_dlc", sourceColumns = eventId)
}
