package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.dlc.{CETSignatures, FundingSignatures}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

case class ExecutedDLCDb(
    eventId: Sha256DigestBE,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    initiatorCetSigsOpt: Option[CETSignatures],
    fundingSigsOpt: Option[FundingSignatures],
    oracleSigOpt: Option[SchnorrDigitalSignature])

class ExecutedDLCTable(tag: Tag)
    extends Table[ExecutedDLCDb](tag, "wallet_dlcs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

  def isInitiator: Rep[Boolean] = column("isInitiator")

  def account: Rep[HDAccount] = column("account")

  def keyIndex: Rep[Int] = column("keyIndex")

  def initiatorCetSigsOpt: Rep[Option[CETSignatures]] =
    column("initiatorCetSigs")

  def fundingSigsOpt: Rep[Option[FundingSignatures]] = column("fundingSigs")

  def oracleSigOpt: Rep[Option[SchnorrDigitalSignature]] = column("oracleSig")

  private type DLCTuple = (
      Sha256DigestBE,
      Boolean,
      HDAccount,
      Int,
      Option[CETSignatures],
      Option[FundingSignatures],
      Option[SchnorrDigitalSignature])

  private val fromTuple: DLCTuple => ExecutedDLCDb = {
    case (eventId,
          isInitiator,
          account,
          keyIndex,
          cetSigsOpt,
          fundingSigsOpt,
          oracleSigOpt) =>
      ExecutedDLCDb(
        eventId,
        isInitiator,
        account,
        keyIndex,
        cetSigsOpt,
        fundingSigsOpt,
        oracleSigOpt
      )
  }

  private val toTuple: ExecutedDLCDb => Option[DLCTuple] = dlc =>
    Some(
      (dlc.eventId,
       dlc.isInitiator,
       dlc.account,
       dlc.keyIndex,
       dlc.initiatorCetSigsOpt,
       dlc.fundingSigsOpt,
       dlc.oracleSigOpt))

  def * : ProvenShape[ExecutedDLCDb] =
    (eventId,
     isInitiator,
     account,
     keyIndex,
     initiatorCetSigsOpt,
     fundingSigsOpt,
     oracleSigOpt) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_dlc", sourceColumns = eventId)
}
