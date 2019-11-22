package org.bitcoins.wallet.models

import org.bitcoins.core.crypto._
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

case class DLCDb(
    eventId: Sha256DigestBE,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    refundSigOpt: Option[PartialSignature],
    oracleSigOpt: Option[SchnorrDigitalSignature])

class DLCTable(tag: Tag) extends Table[DLCDb](tag, "wallet_dlcs") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

  def isInitiator: Rep[Boolean] = column("isInitiator")

  def account: Rep[HDAccount] = column("account")

  def keyIndex: Rep[Int] = column("keyIndex")

  def initiatorRefundSigOpt: Rep[Option[PartialSignature]] =
    column("initiatorRefundSig")

  def oracleSigOpt: Rep[Option[SchnorrDigitalSignature]] = column("oracleSig")

  private type DLCTuple = (
      Sha256DigestBE,
      Boolean,
      HDAccount,
      Int,
      Option[PartialSignature],
      Option[SchnorrDigitalSignature])

  private val fromTuple: DLCTuple => DLCDb = {
    case (eventId,
          isInitiator,
          account,
          keyIndex,
          initiatorRefundSigOpt,
          oracleSigOpt) =>
      DLCDb(
        eventId,
        isInitiator,
        account,
        keyIndex,
        initiatorRefundSigOpt,
        oracleSigOpt
      )
  }

  private val toTuple: DLCDb => Option[DLCTuple] = dlc =>
    Some(
      (dlc.eventId,
       dlc.isInitiator,
       dlc.account,
       dlc.keyIndex,
       dlc.refundSigOpt,
       dlc.oracleSigOpt))

  def * : ProvenShape[DLCDb] =
    (eventId,
     isInitiator,
     account,
     keyIndex,
     initiatorRefundSigOpt,
     oracleSigOpt) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_dlc", sourceColumns = eventId)
}
